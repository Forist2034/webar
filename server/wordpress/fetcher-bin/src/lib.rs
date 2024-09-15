use std::{
    ffi::CStr,
    io::{self, Write},
    ops::Deref,
    process::ExitCode,
};

use anyhow::{Context, Result};
use rustix::{
    fd::{AsFd, BorrowedFd},
    fs::{self, Mode, OFlags},
};

use webar_core::{
    fetch::{
        http::{
            Metadata, TrafficType, DATA_FILE, KEY_LOG_FILE, REQUEST_META_FILE, TRACING_LOG_FILE,
        },
        FetchMeta, META_FILE,
    },
    Timestamp, Version,
};
use webar_data::cbor;
use webar_wordpress_core::{
    source::{FetchType, SERVER},
    Address,
};
use webar_wordpress_fetcher::{client::Config, sink::TarSink, Client, Fetcher};

pub type FileWriter = io::BufWriter<std::fs::File>;

pub type FileFetcher = Fetcher<FileWriter, FileWriter>;

fn open(root: BorrowedFd, name: &CStr) -> rustix::io::Result<std::fs::File> {
    fs::openat(
        root.as_fd(),
        name,
        OFlags::CREATE | OFlags::EXCL | OFlags::WRONLY,
        Mode::from_raw_mode(0o444),
    )
    .map(std::fs::File::from)
}

fn run(
    root: BorrowedFd,
    config: Config,
    https_only: bool,
    instance: &Address<&str, &str>,
    user: (),
    f: impl FnOnce(&mut FileFetcher) -> Result<()>,
) -> Result<()> {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to build runtime")?;
    let mut fetcher = Fetcher {
        seq: 0,
        client: Client {
            config,
            runtime: runtime.handle().clone(),
            client: reqwest::ClientBuilder::new()
                .https_only(https_only)
                .use_preconfigured_tls(webar_rustls::default_config(io::BufWriter::new(
                    open(root, KEY_LOG_FILE.c_path).context("failed to open keylog file")?,
                )))
                .build()
                .context("failed to build http client")?,
        },
        sink: TarSink::new(
            io::BufWriter::new(
                open(root, REQUEST_META_FILE.c_path).context("failed to open metadata file")?,
            ),
            io::BufWriter::new(open(root, DATA_FILE.c_path).context("failed to open data file")?),
        ),
    };
    let start_time = Timestamp::now();

    f(&mut fetcher)?;

    let (meta, data) = fetcher.sink.into_inner().context("failed to close data")?;
    meta.into_inner().context("failed to flush metadata file")?;
    data.into_inner().context("failed to flush data file")?;

    let meta = FetchMeta {
        server: SERVER,
        instance,
        ty: FetchType::RestApi,
        version: Version(1, 0),
        data: Metadata {
            traffic: Some(TrafficType::Wireshark),
            start_time,
            end_time: Timestamp::now(),
            user,
        },
    };
    open(root, META_FILE.c_path)
        .context("failed to open metadata file")?
        .write_all(&cbor::to_vec(&meta))
        .context("failed to write meta file")
}

fn inner_main(
    root: BorrowedFd,
    config: Config,
    https_only: bool,
    instance: &Address<&str, &str>,
    user: (),
    f: impl FnOnce(&mut FileFetcher) -> Result<()>,
) -> Result<()> {
    webar_rustls::global_init();
    webar_tracing::init(open(root, TRACING_LOG_FILE.c_path).context("failed to open log file")?)
        .context("failed to init tracing")?;
    let _span = tracing::info_span!(
        "wordpress",
        instance = tracing::field::debug(instance),
        webar.root = true
    )
    .entered();
    match run(root.as_fd(), config, https_only, instance, user, f) {
        Ok(()) => Ok(()),
        Err(e) => {
            tracing::error!(err = e.deref(), "{e:?}");
            Err(e)
        }
    }
}

pub fn fetch_main(
    dest: &str,
    config: Config,
    https_only: bool,
    instance: &Address<&str, &str>,
    user: (),
    f: impl FnOnce(&mut FileFetcher) -> Result<()>,
) -> ExitCode {
    unsafe {
        webar_traffic_capture::dumpcap_main(dest, |root| {
            inner_main(root, config, https_only, instance, user, f)
        })
    }
}
