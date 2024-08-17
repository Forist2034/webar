use std::{fs, marker::PhantomData, path::Path, process::ExitCode};

use anyhow::{Context, Result};
use serde::de::DeserializeOwned;

use webar_core::{
    digest::Digest,
    object::{encode_object, ObjectId, ObjectInfo, Server},
};
use webar_data::ser::{Never, Serialize};
use webar_media_core::image::fetcher::ImageSpec;

pub trait ServerConfig {
    const SERVER: Server<&'static str>;
    type ImageId: Serialize + DeserializeOwned;

    type Instance: Serialize + Copy;
    type Archive<'a>: Serialize;

    fn to_archive<'a>(id: &'a Self::ImageId) -> Self::Archive<'a>;
}

pub struct Config {
    /// skip if image is fetched
    pub filter_image: bool,
    /// skip if preferred url is fetched
    pub skip_if_fetched_preferred: bool,
}

fn run<S: ServerConfig>(
    config: Config,
    instance: S::Instance,
    root: &str,
    input: &str,
    output: &str,
) -> Result<()> {
    let index = webar_image_store::object::index::Index::open_ro(
        Path::new(root).join(webar_image_store::object::index::INDEX_PATH.path),
    )
    .context("failed to open index")?;

    let specs: Vec<ImageSpec<S::ImageId>> =
        ciborium::from_reader(fs::read(input).context("failed to read input")?.as_slice())
            .context("failed to decode input")?;
    let mut ret = Vec::with_capacity(specs.len());

    for spec in specs {
        if config.filter_image {
            let archive_id: ObjectId<S::Archive<'_>> = ObjectId(
                Digest::digest(&encode_object(
                    &S::SERVER,
                    &ObjectInfo {
                        instance,
                        ty: webar_core::object::ObjectType::Archive::<S::Archive<'_>, Never, Never>,
                        version: 1,
                    },
                    &S::to_archive(&spec.id),
                )),
                PhantomData,
            );
            if index.object_exists(&archive_id)? {
                continue;
            }
        }
        let preferred_fetched = index.url_exists(&spec.preferred_url)?;
        if preferred_fetched && config.skip_if_fetched_preferred {
            continue;
        }
        let mut other_urls = {
            let mut ret = Vec::with_capacity(spec.other_urls.len());
            for u in spec.other_urls {
                if !index.url_exists(&u)? {
                    ret.push(u);
                }
            }
            ret
        };
        if preferred_fetched {
            if other_urls.is_empty() {
                continue;
            } else {
                let preferred_url = other_urls.remove(0);
                ret.push(ImageSpec {
                    id: spec.id,
                    preferred_url,
                    other_urls,
                });
            }
        } else {
            ret.push(ImageSpec {
                id: spec.id,
                preferred_url: spec.preferred_url,
                other_urls,
            })
        }
    }

    std::fs::write(output, webar_data::cbor::to_vec(&ret)).context("failed to write output")?;

    Ok(())
}

pub fn main<S: ServerConfig>(
    config: Config,
    instance: S::Instance,
    root: &str,
    input: &str,
    output: &str,
) -> ExitCode {
    match run::<S>(config, instance, root, input, output) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
