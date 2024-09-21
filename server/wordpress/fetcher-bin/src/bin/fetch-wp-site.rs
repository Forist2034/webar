use std::{process::ExitCode, time::Duration};

use anyhow::{Context, Result};
use chrono::{Datelike, Local};
use clap::Parser;
use serde::{de::DeserializeOwned, Deserialize};
use uuid::Uuid;

use webar_core::{Domain, Host};
use webar_wordpress_core::Address;
use webar_wordpress_fetcher::{
    client,
    handler::{ApiBase, Handler},
    EdgeIter, Error,
};
use webar_wordpress_fetcher_bin::FileFetcher;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ApiConfig {
    Path {
        https: bool,
        domain: String,
        prefix: String,
    },
    Param(String),
    #[serde(rename = "wordpress.com")]
    WordpressCom(String),
}
#[derive(Debug, Deserialize)]
struct Site {
    address: Address<String, String>,
    api_base: ApiConfig,
    list_users: bool,
}
#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum PathFormat {
    /// {year}/{month}/{day}/{uuid}
    DateUuid,
    Uuid,
    Manual(String),
}
#[derive(Debug, Deserialize)]
struct ClientCfg {
    https_only: bool,
    wait_sec: u32,
    retry: u16,
    retry_wait: u32,
}
#[derive(Debug, Deserialize)]
struct Config {
    instance: Address<String, String>,
    sites: Vec<Site>,
    client: ClientCfg,
    root: String,
    #[serde(default)]
    path_format: Option<PathFormat>,
}

#[derive(Debug, clap::Parser)]
struct Cli {
    #[arg(long)]
    config: String,
    /// override path in config file
    #[arg(long)]
    path: Option<String>,
}

fn addr_to_str<'a>(addr: &'a Address<String, String>) -> Address<&'a str, &'a str> {
    Address(
        match &addr.0 {
            Host::Domain(Domain(d)) => Host::Domain(Domain(d.as_str())),
        },
        addr.1.as_str(),
    )
}

fn load_config(args: &Cli) -> Result<(Config, String)> {
    let cfg: Config =
        serde_json::from_slice(&std::fs::read(&args.config).context("failed to read config")?)
            .context("failed to parse config")?;

    let root = &cfg.root;
    let dest_path = match &args.path {
        Some(p) => p.to_string(),
        None => match cfg
            .path_format
            .as_ref()
            .context("setting null in path_format requires specify path from command line")?
        {
            PathFormat::DateUuid => {
                let date = Local::now().date_naive();
                format!(
                    "{root}/{}/{}/{}/{}",
                    date.year(),
                    date.month(),
                    date.day(),
                    Uuid::now_v7()
                )
            }
            PathFormat::Uuid => format!("{root}/{}", Uuid::now_v7()),
            PathFormat::Manual(m) => format!("{root}/{m}"),
        },
    };

    Ok((cfg, dest_path))
}

fn fetch_site(fetcher: &mut FileFetcher, site: &Site) -> Result<()> {
    let handle = Handler {
        blog: addr_to_str(&site.address),
        api_base: match &site.api_base {
            ApiConfig::Path {
                https,
                domain,
                prefix,
            } => ApiBase::path(*https, domain.as_str(), prefix.as_str()),
            ApiConfig::Param(p) => ApiBase::param(p.as_str()),
            ApiConfig::WordpressCom(domain) => ApiBase::wordpress_com(domain.as_str()),
        },
    };

    fn get_all<O: DeserializeOwned>(iter: EdgeIter<O>) -> std::result::Result<(), client::Error> {
        for o in iter {
            o?;
        }
        Ok(())
    }
    fetcher.with_edge_iter(handle.list_pages(), 0, get_all)?;

    fetcher.with_edge_iter(handle.list_posts(), 0, get_all)?;

    fetcher.with_edge_iter(handle.list_categories(), 0, get_all)?;

    fetcher.with_edge_iter(handle.list_tags(), 0, get_all)?;

    match fetcher.with_edge_iter(handle.list_media(), 0, get_all) {
        Ok(()) => (),
        Err(Error::Inner(client::Error::Http(e)))
            if e.status() == Some(reqwest::StatusCode::UNAUTHORIZED) =>
        {
            tracing::warn!("failed to list media");
        }
        Err(e) => return Err(anyhow::Error::new(e)),
    }

    if site.list_users {
        fetcher.with_edge_iter(handle.list_users(), 0, get_all)?;
    }

    Ok(())
}
fn run(fetcher: &mut FileFetcher, config: &Config) -> Result<()> {
    for site in config.sites.iter() {
        match fetch_site(fetcher, site) {
            Ok(()) => (),
            Err(e) => {
                let err_ref: &dyn std::error::Error = e.as_ref();
                tracing::error!(
                    site = tracing::field::debug(site),
                    err = err_ref,
                    "failed to fetch site: {e:?}",
                );
            }
        }
    }
    Ok(())
}

fn main() -> ExitCode {
    let args = Cli::parse();
    let (config, dest) = match load_config(&args) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("failed to load config: {e:?}");
            return ExitCode::FAILURE;
        }
    };

    webar_wordpress_fetcher_bin::fetch_main(
        &dest,
        client::Config {
            wait: Duration::from_secs(config.client.wait_sec as u64),
            retry_count: config.client.retry,
            retry_wait: Duration::from_secs(config.client.retry_wait as u64),
        },
        config.client.https_only,
        &addr_to_str(&config.instance),
        (),
        |f| run(f, &config),
    )
}
