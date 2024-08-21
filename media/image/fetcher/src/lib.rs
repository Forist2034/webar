use std::{ffi::CStr, fmt::Debug, io::Write, marker::PhantomData, process::ExitCode};

use anyhow::{Context as ErrContext, Result};
use clap::Parser;
use reqwest::Client;
use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{self, Mode, OFlags},
};
use serde::de::DeserializeOwned;
use uuid::Uuid;

use webar_core::{
    blob::{BlobId, ImageData},
    digest::Digest,
    fetch::{
        http::{Metadata, LOG_FILE},
        FetchMeta, META_FILE,
    },
    http, Timestamp, Version,
};
use webar_data::ser::Serialize;
use webar_image_store::blob;
use webar_media_core::image::{
    fetcher::{HttpRequest, ImageSpec},
    source::RequestId,
};

#[derive(Parser)]
struct Args {
    #[arg(long)]
    store_db: Option<String>,
    images: String,
    dest: String,
}

struct Context {
    index: Option<blob::index::Index<blob::index::ReadOnly>>,
    store: blob::store::BaseStore,
    runtime: tokio::runtime::Runtime,
    client: Client,
}

struct FetchData {
    request: http::Request<RequestId, String, ()>,
    response: http::Response<(), http::HeaderMap<http::HeaderValue>, ImageData<Vec<u8>>>,
}

#[tracing::instrument(skip(client), err)]
async fn fetch_data(client: &Client, url: String) -> Result<FetchData> {
    const X_REQUEST_ID: reqwest::header::HeaderName =
        reqwest::header::HeaderName::from_static("x-request-id");
    let id = Uuid::new_v4();
    let request = http::Request {
        id: RequestId::XRequestId(id),
        method: reqwest::Method::GET.into(),
        url,
        timestamp: Timestamp::now(),
        body: (),
    };
    tracing::info!("sending request");
    let resp = client
        .get(&request.url)
        .header(X_REQUEST_ID, id.to_string())
        .send()
        .await?
        .error_for_status()?;
    tracing::debug!(response = ?resp, "received response");
    let status = resp.status().into();
    let resp_headers = resp.headers().to_owned().into();
    let body = resp.bytes().await?.to_vec();
    let timestamp = Timestamp::now();
    Ok(FetchData {
        request,
        response: http::Response {
            status,
            id: (),
            timestamp,
            headers: resp_headers,
            body: ImageData(body),
        },
    })
}

fn add_data<I>(ctx: &Context, id: I, data: FetchData) -> Result<HttpRequest<I>> {
    // TODO: hash data only once
    let digest = Digest::digest(data.response.body.as_ref());
    let exists = match &ctx.index {
        Some(idx) => idx
            .exists(&BlobId::<ImageData<Vec<u8>>>(digest, PhantomData))
            .context("failed to check data existence")?,
        None => false,
    };
    if exists {
        tracing::debug!("data is already fetched, skip adding to store");
    } else {
        ctx.store
            .add_blob(&data.response.body)
            .context("failed to add blob data")?;
    }
    Ok(HttpRequest {
        image_id: id,
        request: data.request,
        response: http::Response {
            id: (),
            timestamp: data.response.timestamp,
            status: data.response.status,
            headers: data.response.headers,
            body: digest,
        },
    })
}

fn fetch_image<I: Debug>(ctx: &Context, seq: u32, spec: ImageSpec<I>) -> Result<HttpRequest<I>> {
    let _span = tracing::info_span!("fetch_image", id = ?spec.id, seq).entered();
    match ctx
        .runtime
        .block_on(fetch_data(&ctx.client, spec.preferred_url))
    {
        Ok(d) => add_data(ctx, spec.id, d),
        Err(_) => {
            tracing::info!("trying other urls");
            for u in spec.other_urls {
                if let Ok(d) = ctx.runtime.block_on(fetch_data(&ctx.client, u)) {
                    return add_data(ctx, spec.id, d);
                }
            }
            anyhow::bail!("all request failed");
        }
    }
}

fn open(root: BorrowedFd, path: &CStr) -> Result<std::fs::File> {
    fs::openat(
        root,
        path,
        OFlags::CREATE | OFlags::EXCL | OFlags::WRONLY,
        Mode::from_raw_mode(0o444),
    )
    .context("failed to open file")
    .map(std::fs::File::from)
}

fn init(path: &str) -> Result<OwnedFd> {
    let root = fs::open(path, OFlags::PATH, Mode::empty()).context("failed to open dest dir")?;
    webar_tracing::init(open(root.as_fd(), LOG_FILE.c_path).context("failed to open log")?)
        .context("failed to init tracing")?;
    Ok(root)
}

pub struct Config {
    pub https_only: bool,
    pub user_agent: &'static str,
}

pub trait ServerConfig {
    type Instance: Serialize;
    type FetchType: Serialize;
    type Id: Debug + Serialize + DeserializeOwned;

    const SERVER: &'static str;
    const FETCH_TYPE: Self::FetchType;
}

fn run<S: ServerConfig>(
    root: BorrowedFd,
    cfg: Config,
    args: &Args,
    instance: S::Instance,
) -> Result<()> {
    fs::mkdirat(root, c"data", Mode::from_raw_mode(0o777)).context("failed to create data dir")?;
    let ctx = Context {
        index: match &args.store_db {
            Some(db) => blob::index::Index::open_ro(db)
                .context("failed to open store index")
                .map(Some)?,
            None => None,
        },
        store: blob::store::BaseStore::open_at(root, c"data")
            .context("failed to open data store")?,
        runtime: tokio::runtime::Runtime::new().context("failed to init tokio runtine")?,
        client: reqwest::Client::builder()
            .user_agent(cfg.user_agent)
            .https_only(cfg.https_only)
            .build()
            .context("failed to build clint")?,
    };
    let specs: Vec<_> = ciborium::from_reader(
        std::fs::read(&args.images)
            .context("failed to read spec file")?
            .as_slice(),
    )
    .context("failed to deserialize spec file")?;
    let mut data = tar::Builder::new(std::io::BufWriter::new(
        open(root, c"data.tar").context("failed to open data file")?,
    ));
    let mut header = tar::Header::new_gnu();
    header.set_mode(0o444);
    let start_time = Timestamp::now();

    for (idx, spec) in specs.into_iter().enumerate() {
        match fetch_image::<S::Id>(&ctx, idx as u32, spec) {
            Ok(d) => {
                let dat = webar_data::cbor::to_vec(&d);
                header.set_size(dat.len() as u64);
                data.append_data(&mut header, format!("{idx}.bin"), dat.as_slice())
                    .context("failed to add data")?;
            }
            Err(e) => {
                tracing::error!("failed to fetch image {idx}: {e:?}");
            }
        }
    }
    data.into_inner()
        .context("failed to finish tar")?
        .into_inner()
        .context("failed to flush tar buffer")?;

    open(root, META_FILE.c_path)
        .context("failed to open meta file")?
        .write_all(
            webar_data::cbor::to_vec(&FetchMeta {
                server: S::SERVER,
                instance,
                ty: S::FETCH_TYPE,
                version: Version(1, 0),
                data: Metadata {
                    start_time,
                    end_time: Timestamp::now(),
                    traffic: None,
                    user: (),
                },
            })
            .as_slice(),
        )
        .context("failed to write metadata")?;
    Ok(())
}

pub fn main<S: ServerConfig>(cfg: Config, instance: S::Instance) -> ExitCode {
    let args = Args::parse();
    let root = match init(&args.dest) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e:?}");
            return ExitCode::FAILURE;
        }
    };
    let _span = tracing::info_span!("image_fetcher", webar.root = true).entered();
    match run::<S>(root.as_fd(), cfg, &args, instance) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            tracing::error!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
