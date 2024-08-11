use serde::Deserialize;

use webar_data::ser::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveImage<S> {
    #[serde(rename = "content")]
    Content(S),
}

pub use webar_media_core::image::source::*;
