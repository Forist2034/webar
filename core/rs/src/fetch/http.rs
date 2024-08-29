/*! data is not add to store now, but may change in the future, so
[internal::DigestField] is only exported in the internal module
*/

use serde::Deserialize;

use webar_data::ser::Serialize;

use crate::{digest::Digest, object::ObjectId, FilePath, Timestamp};

pub mod internal {
    use std::{fmt::Debug, marker::PhantomData};

    use serde::Deserialize;
    use webar_data::ser::Serialize;

    use crate::digest::Digest;

    pub struct KeyLog;

    pub struct Log;

    pub struct WiresharkData;

    pub struct RequestMeta;

    pub struct FetchData;

    pub struct DigestField<T>(pub Digest, pub PhantomData<T>);
    impl<T> Debug for DigestField<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }
    impl<T> Clone for DigestField<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone(), PhantomData)
        }
    }
    impl<T> Copy for DigestField<T> {}
    impl<T> Serialize for DigestField<T> {
        fn serialize<S: webar_data::ser::Serializer>(
            &self,
            serializer: S,
        ) -> Result<S::Ok, S::Error> {
            self.0.serialize(serializer)
        }
    }
    impl<'de, T> Deserialize<'de> for DigestField<T> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            Digest::deserialize(deserializer).map(|d| Self(d, PhantomData))
        }
    }
}

pub type KeyLogId = internal::DigestField<internal::KeyLog>;

pub type WiresharkDataId = internal::DigestField<internal::WiresharkData>;

pub type LogId = internal::DigestField<internal::Log>;

pub type RequestMetaId = internal::DigestField<internal::RequestMeta>;

pub type FetchDataId = internal::DigestField<internal::FetchData>;

#[inline]
pub fn id_as_digest<T>(d: &internal::DigestField<T>) -> &Digest {
    &d.0
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Traffic {
    #[serde(rename = "wireshark")]
    Wireshark {
        key_log: KeyLogId,
        request_meta: RequestMetaId,
        data: WiresharkDataId,
    },
    #[serde(rename = "none")]
    None { fetch_data: FetchDataId },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FetchInfo<L> {
    pub start_time: Timestamp,
    pub end_time: Timestamp,
    pub log: LogId,
    pub user: L,
    pub traffic: Traffic,
}

pub type FetchId<L> = ObjectId<FetchInfo<L>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TrafficType {
    #[serde(rename = "wireshark")]
    Wireshark,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata<L> {
    pub traffic: Option<TrafficType>,
    pub start_time: Timestamp,
    pub end_time: Timestamp,
    pub user: L,
}

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
pub const WIRESHARK_LOG_FILE: FilePath = FilePath {
    path: "dumpcap.log",
    c_path: c"dumpcap.log",
};

pub const REQUEST_META_FILE: FilePath = FilePath {
    path: "request_meta.tar",
    c_path: c"request_meta.tar",
};

pub const DATA_FILE: FilePath = FilePath {
    path: "data.tar",
    c_path: c"data.tar",
};
