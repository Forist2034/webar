use std::{fs, io};

use tracing::{level_filters::LevelFilter, Subscriber};
use tracing_subscriber::{fmt, registry::LookupSpan, Layer};

mod write;

pub use tracing_subscriber::{filter, layer::SubscriberExt, registry, util::SubscriberInitExt};

pub fn term_output<S>() -> impl Layer<S>
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fmt::layer().pretty()
}

pub fn init_no_filter(output: fs::File) -> Result<(), io::Error> {
    registry()
        .with(term_output())
        .with(write::WriteLayer::new(output)?)
        .init();
    Ok(())
}

pub fn init(output: fs::File) -> Result<(), io::Error> {
    registry()
        .with(term_output().with_filter(LevelFilter::INFO))
        .with(write::WriteLayer::new(output)?)
        .init();
    Ok(())
}
