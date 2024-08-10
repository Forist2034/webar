use serde::Deserialize;

use webar_data::ser::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveImage {
    #[serde(rename = "content")]
    Content(String),
}