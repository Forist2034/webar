use serde::Deserialize;

use webar_core::digest::Digest;
use webar_data::ser::Serialize;

use super::source;

#[derive(Serialize, Deserialize)]
pub struct ImageSpec<I> {
    pub id: I,
    pub preferred_url: String,
    pub other_urls: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct HttpRequest<I> {
    pub image_id: I,
    pub request: source::Request<String>,
    pub response: source::Response<Digest>,
}
