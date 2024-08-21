use std::process::ExitCode;

use webar_core::Server;
use webar_image_fetcher::ServerConfig;
use webar_stackexchange_core::{
    image::source::ArchiveImage,
    source::{FetchType, SERVER},
};

struct ServerCfg;
impl ServerConfig for ServerCfg {
    type Instance = ();
    type FetchType = FetchType;
    type Id = ArchiveImage<String>;

    const SERVER: Server<&'static str> = SERVER;
    const FETCH_TYPE: Self::FetchType = FetchType::Image;
}

fn main() -> ExitCode {
    webar_image_fetcher::main::<ServerCfg>(
        webar_image_fetcher::Config {
            https_only: true,
            user_agent: "curl",
        },
        (),
    )
}
