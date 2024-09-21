use std::{process::ExitCode, time::Duration};

use anyhow::Context;
use clap::Parser;

use serde::de::DeserializeOwned;
use webar_core::{Domain, Host};
use webar_wordpress_core::{id::PostId, Address};
use webar_wordpress_fetcher::{
    client::{self, Config},
    handler::{ApiBase, Handler},
    EdgeIter, Fetcher,
};
use webar_wordpress_fetcher_bin::FileWriter;

const DOMAIN: &str = "make.wordpress.org";
const PREFIX: &str = "/core";
const INSTANCE: Address<&str, &str> = Address(Host::Domain(Domain("make.wordpress.org")), "/");
const POSTS: [PostId; 1] = [PostId(114959)]; // contains image

#[derive(Clone, Copy, clap::ValueEnum)]
enum Depth {
    Basic,
    Full,
}

#[derive(clap::Parser)]
struct Args {
    /// fetch depth
    depth: Depth,
    dest: String,
}

fn list_all<O: DeserializeOwned>(it: EdgeIter<O>) -> Result<(), client::Error> {
    for r in it.take(2) {
        r?;
    }
    Ok(())
}

fn run(fetcher: &mut Fetcher<FileWriter, FileWriter>, depth: Depth) -> Result<(), anyhow::Error> {
    let handler = Handler {
        blog: Address(Host::Domain(Domain(DOMAIN)), PREFIX),
        api_base: ApiBase::path(true, DOMAIN, PREFIX),
    };

    for p in POSTS {
        fetcher
            .fetch_node(handler.post(p).get())
            .context("failed to get post")?;
    }
    let posts = fetcher
        .with_edge_iter(
            handler.list_posts(),
            0,
            |mut it| -> Result<_, client::Error> {
                let ret = it.next().unwrap()?;
                for p in it.take(2) {
                    p?;
                }
                Ok(ret)
            },
        )
        .context("failed to get posts")?;
    if let Depth::Full = depth {
        for p in posts.data.iter().take(2) {
            let h = handler.post(p.id);
            // fetcher
            //     .with_page_iter(h.list_revisions(), 0, list_all)
            //     .context("failed to get post revisions")?;
            fetcher
                .with_edge_iter(h.list_comments(), 0, list_all)
                .context("failed to get post comments")?;
        }
        fetcher
            .with_edge_iter(handler.list_comments(), 0, list_all)
            .context("failed to get comments")?;
        fetcher
            .with_edge_iter(handler.list_media(), 0, list_all)
            .context("failed to get media")?;
        fetcher
            .with_edge_iter(handler.list_categories(), 0, list_all)
            .context("failed to get categories")?;
        fetcher
            .with_edge_iter(handler.list_tags(), 0, list_all)
            .context("failed to get tags")?;
        fetcher
            .with_edge_iter(handler.list_users(), 0, list_all)
            .context("failed to get users")?;
    }
    Ok(())
}

fn main() -> ExitCode {
    let args = Args::parse();

    webar_wordpress_fetcher_bin::fetch_main(
        &args.dest,
        Config {
            retry_count: 3,
            retry_wait: Duration::from_secs(30),
            wait: Duration::from_secs(5),
        },
        true,
        &INSTANCE,
        (),
        |f| run(f, args.depth),
    )
}
