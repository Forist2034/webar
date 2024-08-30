use serde::Deserialize;
use webar_core::{Server, Version};
use webar_data::ser::Serialize;

pub const SERVER: Server<&'static str> = Server {
    name: "WordPress",
    version: Version(1, 0),
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FetchType {
    #[serde(rename = "rest_api")]
    RestApi,
}
