use serde::Deserialize;

use webar_data::ser::Serialize;

use crate::{FilePath, Version};

pub mod http;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[data(sort_fields = false)]
pub struct FetchMeta<S, I, T, D> {
    pub server: S,
    pub instance: I,
    #[serde(rename = "type")]
    pub ty: T,
    pub version: Version,
    pub data: D,
}

pub const META_FILE: FilePath = FilePath {
    path: "meta.bin",
    c_path: c"meta.bin",
};
