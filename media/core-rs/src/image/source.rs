use serde::Deserialize;
use uuid::Uuid;

use webar_core::{
    blob::{BlobId, ImageData},
    fetch, http,
    object::ObjectId,
    Timestamp,
};
use webar_data::ser::{Never, Serialize};

pub type FetchInfo = fetch::http::FetchInfo<Never>;

pub type FetchId = fetch::http::FetchId<Never>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequestId {
    #[serde(rename = "x-request-id")]
    XRequestId(Uuid),
}

pub type Request<U> = http::Request<RequestId, U, (), ()>;
pub type Response<B> = http::Response<(), B>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpInfo<U> {
    pub fetch: FetchId,
    pub request: Request<U>,
    pub response: Response<BlobId<ImageData<Vec<u8>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RequestRecord {
    #[serde(rename = "fetch")]
    Fetch,
    #[serde(rename = "http_request")]
    HttpRequest,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Content {
    #[serde(rename = "normal")]
    Normal(BlobId<ImageData<Vec<u8>>>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Image {
    pub fetch: FetchId,
    pub request: ObjectId<HttpInfo<String>>,
    pub timestamp: Timestamp,
    pub content: Content,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SnapshotType {
    #[serde(rename = "image")]
    Image,
}
