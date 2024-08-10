use serde::Deserialize;
use uuid::Uuid;

use webar_core::http;
use webar_data::ser::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequestId {
    #[serde(rename = "x-request-id")]
    XRequestId(Uuid),
}

pub type Request<U> = http::Request<RequestId, U, ()>;
pub type Response<B> = http::Response<(), http::HeaderMap<http::HeaderValue>, B>;
