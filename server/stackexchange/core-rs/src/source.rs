use serde::Deserialize;

use webar_core::object::Server;
use webar_data::ser::Serialize;

use crate::image;

pub const SERVER: Server<&'static str> = Server {
    name: "StackExchange",
    version: 1,
};

pub type Host = ();

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecordType {
    #[serde(rename = "image_request")]
    ImageRequest(image::source::RequestRecord),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveInfo {
    #[serde(rename = "image")]
    Image(image::source::ArchiveImage),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SnapshotType {
    #[serde(rename = "image")]
    Image(image::source::SnapshotType),
}
