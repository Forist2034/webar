use std::{env::args_os, fs, path::Path, process::ExitCode};

use anyhow::{Context, Result};

use webar_data::cbor;
use webar_stackexchange_core::rest_api::filter::TypeMap;

use stackexchange_create_filter::FilterConfig;

fn run(config_path: impl AsRef<Path>, dest: impl AsRef<Path>) -> Result<()> {
    let config_data = fs::read(&config_path).context("failed to read config")?;
    let config: TypeMap<FilterConfig<'_>> =
        serde_json::from_slice(&config_data).context("failed to parse config")?;

    let client = reqwest::blocking::ClientBuilder::new()
        .https_only(true)
        .build()
        .context("failed to create client")?;

    let api_result = config
        .try_map_ref(|cfg| match cfg.create(&client) {
            Ok(r) => {
                println!("created filter: {}", r.1.filter);
                Ok(r.0)
            }
            Err(e) => Err(e),
        })
        .context("failed to create filters")?;

    fs::write(dest, cbor::to_vec(&api_result)).context("failed to write dest")?;

    Ok(())
}

fn main() -> ExitCode {
    let (config_path, dest) = {
        let mut a = args_os();
        a.next().unwrap();
        (
            a.next().expect("config file path"),
            a.next().expect("dest path"),
        )
    };
    match run(config_path, dest) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
