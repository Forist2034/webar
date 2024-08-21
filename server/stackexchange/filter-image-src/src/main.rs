use clap::Parser;

use webar_stackexchange_core::{image, source};

use webar_image_filter_source::{Config, ServerConfig};

#[derive(Debug, Parser)]
struct Args {
    #[arg(long)]
    root: String,
    input: String,
    output: String,
}

struct ServerCfg;
impl ServerConfig for ServerCfg {
    const SERVER: webar_core::Server<&'static str> = source::SERVER;

    type ImageId = image::source::ArchiveImage<String>;
    type Instance = source::Instance;
    type Archive<'a> = source::ArchiveInfo<&'a str>;

    fn to_archive<'a>(id: &'a Self::ImageId) -> Self::Archive<'a> {
        source::ArchiveInfo::Image(image::source::ArchiveImage::Content(match id {
            image::source::ArchiveImage::Content(c) => c,
        }))
    }
}

fn main() -> std::process::ExitCode {
    let args = Args::parse();
    webar_image_filter_source::main::<ServerCfg>(
        Config {
            filter_image: true,
            skip_if_fetched_preferred: true,
        },
        (),
        &args.root,
        &args.input,
        &args.output,
    )
}
