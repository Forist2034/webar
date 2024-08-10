pub mod id {
    use std::fmt::Display;

    use serde::Deserialize;
    use uuid::Uuid;

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
    macro_rules! wrap_str {
        ($i:ident) => {
            #[derive(
                Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
            )]
            #[serde(transparent)]
            pub struct $i<S>(pub S);

            impl<S: AsRef<str>> Display for $i<S> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    self.0.as_ref().fmt(f)
                }
            }
        };
    }

    wrap_copy!(AnswerId(u64));

    wrap_copy!(BadgeId(u64));

    wrap_str!(CollectiveSlug);

    wrap_copy!(CommentId(u64));

    wrap_copy!(QuestionId(u64));

    wrap_copy!(PostId(u64));
    impl From<AnswerId> for PostId {
        fn from(value: AnswerId) -> Self {
            Self(value.0)
        }
    }
    impl From<QuestionId> for PostId {
        fn from(value: QuestionId) -> Self {
            Self(value.0)
        }
    }

    wrap_copy!(RevisionId(Uuid));

    wrap_str!(TagName);

    wrap_copy!(UserId(i64));

    wrap_copy!(AccountId(i64));
}

mod site;
pub use site::KnownSite;

pub mod api {
    use serde::Deserialize;

    use webar_data::ser::Serialize;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
    #[non_exhaustive]
    pub enum ApiVersion {
        #[serde(rename = "2.3")]
        V2_3,
    }

    pub mod filter;
    pub mod model;
    pub mod request;
}

pub mod fetcher {
    pub mod api_client;
}

pub mod image {
    pub mod source;
}
