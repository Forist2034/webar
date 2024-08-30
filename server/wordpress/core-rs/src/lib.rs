use serde::Deserialize;

use webar_core::Host;
use webar_data::ser::Serialize;

pub mod id {
    use std::fmt::Display;

    use serde::Deserialize;

    use webar_data::ser::Serialize;

    macro_rules! wrap_copy {
        ($i:ident($t:ty)) => {
            #[derive(
                Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
            )]
            #[serde(transparent)]
            pub struct $i(pub $t);
            impl Display for $i {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    self.0.fmt(f)
                }
            }
        };
    }

    wrap_copy!(CategoryId(u64));
    wrap_copy!(CommentId(u64));
    wrap_copy!(MediaId(u64));

    wrap_copy!(PageId(u64));
    wrap_copy!(PageRevisionId(u64));
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct PageRevisionHandle(pub PageId, pub PageRevisionId);

    wrap_copy!(PostId(u64));
    wrap_copy!(PostRevisionId(u64));

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct PostRevisionHandle(pub PostId, pub PostRevisionId);

    wrap_copy!(TagId(u64));
    wrap_copy!(UserId(u64));
}

pub mod rest_api {
    use serde::Deserialize;
    use webar_data::ser::Serialize;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    pub enum ApiVersion {
        #[serde(rename = "2")]
        V2,
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ApiInfo {
        pub version: ApiVersion,
    }

    pub mod model;
    pub mod source;
}

/// wordpress address (host, path)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Address<D, P>(pub Host<D>, pub P);

pub mod fetcher {
    pub mod rest_client;
}

pub mod source;
