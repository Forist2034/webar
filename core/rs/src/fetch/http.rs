use serde::Deserialize;

use webar_data::ser::Serialize;

use crate::{digest::Digest, FilePath, Timestamp};

/// digest of info.json
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FetchId(pub Digest);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Traffic {
    #[serde(rename = "wireshark")]
    Wireshark {
        key_log: Digest,
        /// metadata of request, used to extract data from captured packet
        request_meta: Digest,
        data: Digest,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FetchInfo<L> {
    pub timestamp: Timestamp,
    pub log: Digest,
    pub user: Option<L>,
    pub key_log: Option<Digest>,
    pub traffic: Traffic,
}

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
