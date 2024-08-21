use std::{
    env::args,
    ffi::CStr,
    io::{self, Write},
    num::NonZeroUsize,
    ops::Deref,
    path::{Path, PathBuf},
    process::ExitCode,
};

use anyhow::Context;
use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{Mode, OFlags},
};
use webar_core::{
    fetch::{
        http::{Metadata, KEY_LOG_FILE, LOG_FILE, REQUEST_META_FILE},
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
    client::{
        self, AnswerHandler, AnswersHandler, GetInfo, QuestionHandler, QuestionsHandler,
        TagHandler, TagsHandler, UserHandler, UsersHandler,
    },
    sink::TarSink,
    Client, Fetcher, ManyChunk, NonEmpty, HS_FILTER_INFO,
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

fn init(sink_path: &Path) -> anyhow::Result<OwnedFd> {
    let root = rustix::fs::open(sink_path, OFlags::PATH, Mode::all())
        .context("failed to open dest dir")?;
    webar_tracing::init(open(root.as_fd(), LOG_FILE.c_path).context("failed to open log file")?)
        .context("failed to init tracing log")?;
    Ok(root)
}

fn run(full: bool, root: BorrowedFd<'_>) -> anyhow::Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to build tokio runtime")?;
    let mut fetcher = Fetcher {
        client: Client {
            seq: 0,
            site: KnownSite::MetaStackExchange,
            filter: HS_FILTER_INFO,
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

    fetcher
        .fetch_object(GetInfo)
        .context("failed to get info")?;

    let questions = QuestionsHandler::<[QuestionId; 0]>(NonEmpty(QUESTION_ID, []));
    let question = QuestionHandler(QUESTION_ID);
    let q = fetcher
        .fetch_object(questions.get())
        .context("failed to fetch question")?;

    if full {
        const START: NonZeroUsize = match NonZeroUsize::new(1) {
            Some(v) => v,
            None => unreachable!(),
        };

        // tags
        for t in ManyChunk::<20, _>(
            q.parsed.items[0]
                .tags
                .iter()
                .map(|t| TagName(t.0.as_str()))
                .fuse(),
        ) {
            let t = TagsHandler(t);
            fetcher.fetch_object(t.get()).context("failed to get tag")?;
            fetcher
                .fetch_object(t.wikis())
                .context("failed to get tag wiki")?;
            for th in t.0.iter().copied().map(TagHandler) {
                fetcher
                    .with_list_iter::<_, _, client::Error>(th.synonyms(), START, |it| {
                        for s in it {
                            s?;
                        }
                        Ok(())
                    })
                    .context("failed to get tag synonym")?;
            }
        }

        // question revision
        fetcher
            .with_list_iter::<_, _, client::Error>(question.revisions(), START, |it| {
                for r in it.take(5) {
                    r?;
                }
                Ok(())
            })
            .context("failed to get revision")?;

        fetcher
            .with_list_iter::<_, _, client::Error>(question.comments(), START, |it| {
                for c in it.take(3) {
                    c?;
                }
                Ok(())
            })
            .context("failed to write comment")?;

        let ans = fetcher
            .with_list_iter::<_, _, client::Error>(question.answers(), START, |mut it| {
                Ok([it.next().unwrap()?, it.next().unwrap()?])
            })
            .context("failed to get answer")?
            .0;

        // answers
        for ans in
            ManyChunk::<20, _>(ans.iter().flat_map(|r| r.items.iter().map(|i| i.answer_id))).take(1)
        {
            let ans = AnswersHandler(ans);
            fetcher
                .fetch_object(ans.get())
                .context("failed to get answer")?;
            fetcher
                .fetch_object(ans.questions())
                .context("failed to get answer question")?;
            for ah in ans.0.iter().copied().map(AnswerHandler) {
                fetcher
                    .with_list_iter::<_, _, client::Error>(ah.revisions(), START, |it| {
                        for r in it.take(2) {
                            r?;
                        }
                        Ok(())
                    })
                    .context("failed to get answer revision")?;
            }
        }

        // users
        for us in ManyChunk::<20, _>(ans.iter().flat_map(|r| {
            r.items
                .iter()
                .filter_map(|i| i.last_editor.as_ref().and_then(|e| e.user_id))
        }))
        .take(1)
        {
            let us = UsersHandler(us);
            fetcher
                .fetch_object(us.get())
                .context("failed to get users")?;
            for uh in us.0.iter().copied().map(UserHandler) {
                fetcher
                    .with_list_iter::<_, _, client::Error>(uh.answers(), START, |it| {
                        for a in it.take(2) {
                            a?;
                        }
                        Ok(())
                    })
                    .context("failed to get user answers")?;
                fetcher
                    .with_list_iter::<_, _, client::Error>(uh.badges(), START, |it| {
                        for b in it.take(2) {
                            b?;
                        }
                        Ok(())
                    })
                    .context("failed to get user badges")?;
                fetcher
                    .with_list_iter::<_, _, client::Error>(uh.questions(), START, |it| {
                        for q in it.take(2) {
                            q?;
                        }
                        Ok(())
                    })
                    .context("failed to get user questions")?;
                fetcher
                    .with_list_iter::<_, _, client::Error>(uh.comments(), START, |it| {
                        for c in it.take(2) {
                            c?;
                        }
                        Ok(())
                    })
                    .context("failed to get user comments")?;
            }
        }
    }

    let (meta, data) = fetcher.sink.into_inner().context("failed to close sink")?;
    meta.into_inner().context("failed to flush meta file")?;
    data.into_inner().context("failed to flush data file")?;

    let meta = FetchMeta {
        server: SERVER.name,
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
            PathBuf::from(a.next().expect("missing sink path")),
        )
    };
    webar_rustls::global_init();
    let root = match init(sink_path.as_path()) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{e:?}");
            return ExitCode::FAILURE;
        }
    };
    let _span = tracing::info_span!("stackexchange", webar.root = true).entered();
    match run(full, root.as_fd()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            tracing::error!(err = e.deref(), "failed to fetch data");
            ExitCode::FAILURE
        }
    }
}
