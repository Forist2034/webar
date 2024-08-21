use serde::Deserialize;

use webar_core::{object::Server, Version};
use webar_data::ser::Serialize;

use crate::image;

pub const SERVER: Server<&'static str> = Server {
    name: "StackExchange",
    version: Version(1, 0),
};

pub type Instance = ();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FetchType {
    #[serde(rename = "rest_api")]
    RestApi,
    #[serde(rename = "image")]
    Image,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecordType {
    #[serde(rename = "image_request")]
    ImageRequest(image::source::RequestRecord),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveInfo<S> {
    #[serde(rename = "image")]
    Image(image::source::ArchiveImage<S>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SnapshotType {
    #[serde(rename = "image")]
    Image(image::source::SnapshotType),
}
