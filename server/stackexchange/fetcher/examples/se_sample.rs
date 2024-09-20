use std::{
    env::args,
    ffi::CStr,
    io::{self, Write},
    num::NonZeroUsize,
    ops::Deref,
    process::ExitCode,
};

use anyhow::Context;
use rustix::{
    fd::{AsFd, BorrowedFd},
    fs::{Mode, OFlags},
};
use serde::de::DeserializeOwned;
use webar_core::{
    fetch::{
        http::{Metadata, KEY_LOG_FILE, REQUEST_META_FILE, TRACING_LOG_FILE},
        FetchMeta, META_FILE,
    },
    Timestamp, Version,
};
use webar_stackexchange_core::{
    id::{QuestionId, TagName},
    source::{FetchType, SERVER},
    KnownSite,
};
use webar_stackexchange_fetcher::{
    client, sink::TarSink, Client, EdgeIter, Fetcher, Handler, HS_FILTER_INFO,
};

// formatting sandbox
const QUESTION_ID: QuestionId = QuestionId(3122);

fn open(root: BorrowedFd<'_>, name: &CStr) -> Result<std::fs::File, rustix::io::Errno> {
    rustix::fs::openat(
        root,
        name,
        OFlags::WRONLY | OFlags::CREATE | OFlags::EXCL,
        Mode::from_raw_mode(0o644), // for sample only, set file to readonly when using
    )
    .map(std::fs::File::from)
}

fn get_all<O: DeserializeOwned>(iter: EdgeIter<O>) -> Result<(), client::Error> {
    for v in iter {
        v?;
    }
    Ok(())
}
fn get_first_2<O: DeserializeOwned>(iter: EdgeIter<O>) -> Result<(), client::Error> {
    for v in iter.take(2) {
        v?;
    }
    Ok(())
}

fn run(full: bool, root: BorrowedFd<'_>) -> anyhow::Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to build tokio runtime")?;
    let mut fetcher = Fetcher {
        seq: 0,
        client: Client {
            runtime: rt.handle().clone(),
            client: reqwest::ClientBuilder::new()
                .use_preconfigured_tls(webar_rustls::default_config(io::BufWriter::new(
                    open(root, KEY_LOG_FILE.c_path).context("failed to open keylog file")?,
                )))
                .user_agent("webar")
                .https_only(true)
                .build()
                .context("failed to build client")?,
        },
        sink: TarSink::new(
            io::BufWriter::new(
                open(root, REQUEST_META_FILE.c_path).context("failed to open meta file")?,
            ),
            io::BufWriter::new(open(root, c"data.tar").context("failed to open data file")?),
        ),
    };
    let start_time = Timestamp::now();

    let handler = Handler {
        site: KnownSite::MetaStackExchange,
        filters: HS_FILTER_INFO,
    };

    fetcher
        .fetch_node(handler.get_info())
        .context("failed to get info")?;

    let question = handler.question(QUESTION_ID);
    let q = fetcher
        .fetch_node(question.get())
        .context("failed to fetch question")?
        .data
        .items
        .context("missing question body")?;

    if full {
        const START: NonZeroUsize = match NonZeroUsize::new(1) {
            Some(v) => v,
            None => unreachable!(),
        };

        for TagName(ref tn) in q.tags.iter() {
            let t = handler.tag(TagName(tn.as_str()));
            fetcher.fetch_node(t.get()).context("failed to get tag")?;
            fetcher
                .fetch_node(t.get_wiki())
                .context("failed to get tag wiki")?;
            fetcher
                .with_edge_iter(t.list_synonyms(), START, get_all)
                .context("failed to get tag synonym")?;
        }

        // question revision
        fetcher
            .fetch_revision(question.list_revisions())
            .context("failed to get revisions")?;

        fetcher
            .with_edge_iter(question.list_comments(), START, get_first_2)
            .context("failed to get comments")?;

        let ans = fetcher
            .with_edge_iter(
                question.list_answers(),
                START,
                |mut it| -> Result<_, client::Error> {
                    let mut ret = it.next().unwrap()?.items;
                    ret.extend(it.next().unwrap()?.items);
                    Ok(ret)
                },
            )
            .context("failed to get answer")?
            .0;

        // answers
        for ans in ans.iter().take(10) {
            let ans = handler.answer(ans.answer_id);
            fetcher
                .fetch_node(ans.get())
                .context("failed to get answer")?;
            fetcher
                .fetch_revision(ans.list_revisions())
                .context("failed to get answer revision")?;
        }

        // users
        for us in ans
            .iter()
            .filter_map(|a| a.last_editor.as_ref().and_then(|e| e.user_id))
            .take(10)
        {
            let us = handler.user(us);
            fetcher
                .fetch_node(us.get())
                .context("failed to get users")?;

            fetcher
                .with_edge_iter(us.list_answers(), START, get_first_2)
                .context("failed to get user answers")?;
            fetcher
                .with_edge_iter(us.list_badges(), START, get_first_2)
                .context("failed to get user badges")?;
            fetcher
                .with_edge_iter(us.list_comments(), START, get_first_2)
                .context("failed to get user comments")?;
            fetcher
                .with_edge_iter(us.list_questions(), START, get_first_2)
                .context("failed to get user questions")?;
        }
    }

    let (meta, data) = fetcher.sink.into_inner().context("failed to close sink")?;
    meta.into_inner().context("failed to flush meta file")?;
    data.into_inner().context("failed to flush data file")?;

    let meta = FetchMeta {
        server: SERVER,
        instance: (),
        ty: FetchType::RestApi,
        version: Version(1, 0),
        data: Metadata {
            start_time,
            end_time: Timestamp::now(),
            traffic: Some(webar_core::fetch::http::TrafficType::Wireshark),
            user: (),
        },
    };
    open(root, META_FILE.c_path)
        .context("failed to open meta file")?
        .write_all(&webar_data::cbor::to_vec(&meta))
        .context("failed to write fetch metadata")?;
    Ok(())
}

fn inner_main(root: BorrowedFd, full: bool) -> anyhow::Result<()> {
    webar_rustls::global_init();
    webar_tracing::init(
        open(root.as_fd(), TRACING_LOG_FILE.c_path).context("failed to open log file")?,
    )
    .context("failed to init tracing log")?;
    let _span = tracing::info_span!("stackexchange", webar.root = true).entered();
    match run(full, root.as_fd()) {
        Ok(()) => Ok(()),
        Err(err) => {
            tracing::error!(err = err.deref(), "failed to fetch data");
            Err(err)
        }
    }
}

pub fn main() -> ExitCode {
    let (full, sink_path) = {
        let mut a = args();
        a.next();
        (
            match a.next().expect("missing fetch data").as_str() {
                "basic" => false,
                "full" => true,
                _ => panic!("invalid fetch data"),
            },
            a.next().expect("missing sink path"),
        )
    };

    unsafe {
        webar_traffic_capture::dumpcap_main(sink_path.as_str(), |root| inner_main(root, full))
    }
}
