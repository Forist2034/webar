use std::{fmt::Debug, io::Read, marker::PhantomData, path::Path, process::ExitCode};

use anyhow::{Context as ErrContext, Result};
use rustix::{
    fd::AsFd,
    fs::{self, Mode, OFlags},
};
use serde::de::DeserializeOwned;

use webar_core::{blob::BlobId, fetch::http::DATA_FILE, object::Server, Version};
use webar_data::ser::Serialize;
use webar_media_core::image::{
    fetcher::HttpRequest,
    source::{Content, FetchId, HttpInfo, Image, RequestRecord, Response, SnapshotType},
};
extern crate webar_image_store as store;
use store::fetch::http::read_fetch;

pub trait ServerConfig {
    const SERVER: Server<&'static str>;

    type ImageId: Serialize + DeserializeOwned;
    type Instance: Serialize + DeserializeOwned;
    type InstanceRef<'a>: Serialize + Copy;
    type FetchType: Debug + Eq + DeserializeOwned + Send + Sync + 'static;
    type Archive: Debug + Serialize;
    type Snapshot: Serialize;
    type Record: Serialize;

    const FETCH_TYPE: Self::FetchType;

    fn instance_ref<'a>(instance: &'a Self::Instance) -> Self::InstanceRef<'a>;
    fn to_snapshot(ty: SnapshotType) -> Self::Snapshot;
    fn to_record(record: RequestRecord) -> Self::Record;
    fn to_archive(image_id: Self::ImageId) -> Self::Archive;
}

struct Context<'a, S: ServerConfig> {
    fetch_id: FetchId,
    fetch_blob: store::blob::store::BaseStore,
    blob_store: store::blob::store::WebsiteStore<'a>,
    blob_index: store::blob::index::Index<store::blob::index::ReadWrite>,
    object_store: store::object::store::WebsiteStore<
        'a,
        S::InstanceRef<'a>,
        S::Archive,
        S::Snapshot,
        S::Record,
    >,
    object_index: store::object::index::Index<store::object::index::ReadWrite>,
}

fn add_image<'a, S: ServerConfig>(ctx: &Context<'a, S>, seq: usize, data: &[u8]) -> Result<()> {
    let image: HttpRequest<S::ImageId> =
        ciborium::from_reader(data).context("failed to decode cbor")?;
    let blob_id = BlobId(image.response.body, PhantomData);
    let http_info = HttpInfo {
        fetch: ctx.fetch_id,
        request: image.request,
        response: Response {
            id: image.response.id,
            timestamp: image.response.timestamp,
            status: image.response.status,
            headers: image.response.headers,
            body: blob_id,
        },
    };
    let request_id = ctx
        .object_store
        .add_object(
            webar_core::object::ObjectType::Record(S::to_record(RequestRecord::HttpRequest)),
            Version(1, 0),
            &http_info,
        )
        .context("failed to add http info")?
        .id;
    let archive = S::to_archive(image.image_id);
    let archive_id = ctx
        .object_store
        .add_object(
            webar_core::object::ObjectType::Archive,
            Version(1, 0),
            &archive,
        )
        .context("failed to add archive")?
        .id;
    let snapshot_id = ctx
        .object_store
        .add_object(
            webar_core::object::ObjectType::Snapshot {
                archive: archive_id,
                ty: S::to_snapshot(SnapshotType::Image),
            },
            Version(1, 0),
            &Image {
                fetch: ctx.fetch_id,
                request: request_id,
                timestamp: http_info.response.timestamp,
                content: Content::Normal(blob_id),
            },
        )
        .context("failed to add snapshot")?
        .id;

    if ctx
        .blob_index
        .exists(&blob_id)
        .context("failed to query blob index")?
    {
        // link store blob to fetch so that fetch data contains complete
        // fetched data
        ctx.fetch_blob
            .link(ctx.blob_store.base, &blob_id)
            .context("failed to link blob to fetch")?;
    } else {
        ctx.blob_store
            .link(&ctx.fetch_blob, &blob_id)
            .context("failed to link blob to store")?;
        ctx.blob_index
            .insert(&blob_id)
            .context("failed to insert blob index")?;
    }

    ctx.object_index
        .insert_url(&http_info.request.url)
        .context("failed to add url to index")?;
    ctx.object_index
        .insert_object(&archive_id)
        .context("failed to add snapshot to index")?;

    println!(
        "[{seq}]: {archive:?}\n{}\n{}\n{}",
        format_args!("    archive_id: {archive_id:?}"),
        format_args!("    request_id: {request_id:?}"),
        format_args!("    snapshot_id: {snapshot_id:?}")
    );
    Ok(())
}

fn run<'a, S: ServerConfig>(root_path: &Path, server_path: &str, fetch_root: &str) -> Result<()> {
    let root = fs::open(root_path, OFlags::PATH | OFlags::DIRECTORY, Mode::empty())
        .context("failed to open root")?;
    let server_root = fs::open(server_path, OFlags::PATH | OFlags::DIRECTORY, Mode::empty())
        .context("failed to open server root")?;
    let fetch_root = fs::open(fetch_root, OFlags::PATH | OFlags::DIRECTORY, Mode::empty())
        .context("failed to open fetch root")?;
    let blob_base = store::blob::store::BaseStore::open_at(root.as_fd(), c"store/blob")
        .context("failed to open base blob store")?;
    let object_base = store::object::store::BaseStore::open_at(root.as_fd(), c"store/object")
        .context("failed to open base object store")?;
    let blob_store =
        store::blob::store::WebsiteStore::open_at(&blob_base, server_root.as_fd(), c"store/blob")
            .context("failed to open website blob store")?;
    let fetch = read_fetch(
        S::SERVER.name,
        fetch_root.as_fd(),
        S::FETCH_TYPE,
        &blob_store,
    )
    .context("failed to read fetch")?;
    let object_store = store::object::store::WebsiteStore::open_at(
        S::SERVER,
        S::instance_ref(&fetch.instance),
        &object_base,
        server_root.as_fd(),
        c"store/object",
    )
    .context("failed to open website object store")?;

    let context: Context<S> = Context {
        fetch_id: object_store
            .add_object(
                webar_core::object::ObjectType::Record(S::to_record(RequestRecord::Fetch)),
                Version(1, 0),
                &fetch.info,
            )
            .context("failed to add fetch")?
            .id,
        fetch_blob: store::blob::store::BaseStore::open_at(fetch_root.as_fd(), c"data")
            .context("failed to open fetch data")?,
        blob_store,
        blob_index: store::blob::index::Index::create(
            root_path.join(store::blob::index::INDEX_PATH.path),
        )
        .context("failed to open blob index")?,
        object_store,
        object_index: store::object::index::Index::create(
            root_path.join(store::object::index::INDEX_PATH.path),
        )
        .context("failed to open image index")?,
    };

    let mut data_tar = tar::Archive::new(std::io::BufReader::new(std::fs::File::from(
        fs::openat(fetch_root, DATA_FILE.c_path, OFlags::RDONLY, Mode::empty())
            .context("failed to open data file")?,
    )));

    for (idx, entry) in data_tar
        .entries()
        .context("failed to read entries")?
        .enumerate()
    {
        let mut entry = entry.with_context(|| format!("failed to read entry {idx}"))?;
        let data = {
            let mut buf = Vec::with_capacity(entry.size() as usize);
            entry
                .read_to_end(&mut buf)
                .with_context(|| format!("failed to read entry {idx} content"))?;
            buf
        };
        add_image(&context, idx, &data).with_context(|| format!("failed to add entry {idx}"))?;
    }

    Ok(())
}

pub fn main<S: ServerConfig>(root: &str, server_root: &str, fetch_root: &str) -> ExitCode {
    match run::<S>(root.as_ref(), server_root, fetch_root) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
