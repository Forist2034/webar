use serde::Deserialize;

use webar_data::ser::Serialize;

use crate::{FilePath, Timestamp};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FetchMeta<L> {
    pub timestamp: Timestamp,
    pub user: Option<L>,
}
pub const META_FILE: FilePath = FilePath {
    path: "meta.json",
    c_path: c"meta.json",
};

pub const INFO_FILE: FilePath = FilePath {
    path: "info.bin",
    c_path: c"info.bin",
};

pub const LOG_FILE: FilePath = FilePath {
    path: "log.bin",
    c_path: c"log.bin",
};

pub const KEY_LOG_FILE: FilePath = FilePath {
    path: "sslkeylog.keys",
    c_path: c"sslkeylog.keys",
};

pub const WIRESHARK_DATA_FILE: FilePath = FilePath {
    path: "traffic.pcapng",
    c_path: c"traffic.pcapng",
};

pub const REQUEST_META_FILE: FilePath = FilePath {
    path: "request_meta.tar",
    c_path: c"request_meta.tar",
};
