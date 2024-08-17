use std::process::ExitCode;

use clap::Parser;

use webar_core::object::Server;
use webar_image_add_fetch::ServerConfig;
use webar_stackexchange_core::{image::source::RequestRecord, source};

#[derive(Debug, clap::Parser)]
struct Args {
    root: String,
    server_root: String,
    fetch_root: String,
}

struct ServerCfg;
impl ServerConfig for ServerCfg {
    const SERVER: Server<&'static str> = source::SERVER;

    type ImageId = webar_stackexchange_core::image::source::ArchiveImage<String>;
    type Instance = source::Instance;
    type Archive = source::ArchiveInfo<String>;
    type Snapshot = source::SnapshotType;
    type Record = source::RecordType;

    #[inline]
    fn to_record(record: RequestRecord) -> Self::Record {
        source::RecordType::ImageRequest(record)
    }
    #[inline]
    fn to_snapshot(ty: webar_stackexchange_core::image::source::SnapshotType) -> Self::Snapshot {
        source::SnapshotType::Image(ty)
    }
    #[inline]
    fn to_archive(image_id: Self::ImageId) -> Self::Archive {
        source::ArchiveInfo::Image(image_id)
    }
}

fn main() -> ExitCode {
    let args = Args::parse();
    webar_image_add_fetch::main::<ServerCfg>((), &args.root, &args.server_root, &args.fetch_root)
}
