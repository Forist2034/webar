use std::{fmt::Display, iter::once};

use filter::FilterId;
use serde::Deserialize;
use uuid::Uuid;

use webar_core::{digest::Digest, fetch::http::FetchId, Timestamp};
use webar_data::ser::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ApiVersion {
    #[serde(rename = "2.3")]
    V2_3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ObjectType {
    Answer,
    Badge,
    Collective,
    Comment,
    Info,
    Question,
    Revision,
    // Site,
    Tag,
    TagSynonym,
    TagWiki,
    User,
}
impl ObjectType {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Answer => "answer",
            Self::Badge => "badge",
            Self::Collective => "collective",
            Self::Comment => "comment",
            Self::Info => "info",
            Self::Question => "question",
            Self::Revision => "revision",
            // Self::Site => "site",
            Self::Tag => "tag",
            Self::TagSynonym => "tag_synonym",
            Self::TagWiki => "tag_wiki",
            Self::User => "user",
        }
    }
}
impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
impl Serialize for ObjectType {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

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

macro_rules! list_req {
    (enum $n:ident { $($r:ident = $l:literal,)* }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        pub enum $n {
            $(#[serde(rename= $l)] $r),+
        }
        impl $n {
            pub const fn as_str(&self)-> &'static str {
                match self {
                    $(Self::$r => $l),+
                }
            }
        }
    };
}

list_req!(
    enum AnswerListReq {
        Comment = "comment",
        Revision = "revision",
    }
);
list_req!(
    enum CollectiveListReq {
        Answer = "answer",
        Question = "question",
        Tag = "tag",
        User = "user",
    }
);
list_req!(
    enum QuestionListReq {
        Answer = "answer",
        Comment = "comment",
        Revision = "revision",
    }
);
list_req!(
    enum TagListReq {
        TagSynonym = "tag_synonym",
    }
);
list_req!(
    enum UserListReq {
        Answer = "answer",
        Badge = "badge",
        Comment = "comment",
        Question = "question",
    }
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmpty<T, I>(pub T, pub I);
impl<T, I: AsRef<[T]>> NonEmpty<T, I> {
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.1.as_ref().into_iter().chain(once(&self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ListRequest<S> {
    #[serde(rename = "answer")]
    Answer {
        id: id::AnswerId,
        request: AnswerListReq,
    },
    #[serde(rename = "collective")]
    Collective {
        id: id::CollectiveSlug<S>,
        request: CollectiveListReq,
    },
    #[serde(rename = "list_revision")]
    ListRevision(id::RevisionId),
    #[serde(rename = "question")]
    Question {
        id: id::QuestionId,
        request: QuestionListReq,
    },
    #[serde(rename = "tag")]
    Tag {
        id: id::TagName<S>,
        request: TagListReq,
    },
    #[serde(rename = "user")]
    User {
        id: id::UserId,
        request: UserListReq,
    },
}

pub mod api;

pub mod fetcher {
    pub mod api_client;
}

pub mod filter;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RequestId {
    #[serde(rename = "x-request-id")]
    /// `x-request-id` header
    XRequestId(Uuid),
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ResponseId {
    #[serde(rename = "x-request-guid")]
    /// `x-request-guid` header
    XRequestGuid(Uuid),
}

pub type Response<H, B> = webar_core::http::Response<Option<ResponseId>, H, B>;

#[derive(Debug, Serialize, Deserialize)]
pub struct HttpInfo<U, H> {
    pub url: U,
    pub fetch: FetchId,
    pub call_seq: u32,
    pub response_index: u32,
    pub request_id: RequestId,
    pub response: Response<H, Digest>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct HttpResponseId(pub Digest);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct List<LR, R> {
    pub request: LR,
    pub full: bool,
    pub responses: Vec<R>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Objects<R> {
    #[serde(rename = "type")]
    pub ty: ObjectType,
    pub response: R,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResponseData<O, L> {
    #[serde(rename = "objects")]
    Objects(O),
    #[serde(rename = "list")]
    List(L),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiInfo<LR> {
    pub fetch: FetchId,
    pub call_seq: u32,
    pub version: ApiVersion,
    pub site: KnownSite,
    pub timestamp: Timestamp,
    pub filter: FilterId,
    pub response: ResponseData<Objects<HttpResponseId>, List<LR, HttpResponseId>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ApiResponseId(pub Digest);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SnapshotId {
    pub api_response: ApiResponseId,
    pub index: Option<u32>,
}
impl Display for SnapshotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.api_response.0 {
            Digest::Sha256(s) => write!(f, "sha256-{s}")?,
        };
        match self.index {
            Some(idx) => write!(f, "_{idx}"),
            None => Ok(()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Content {
    #[serde(rename = "content")]
    Normal(Digest),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    pub id: SnapshotId,
    pub content: Content,
    pub api_version: ApiVersion,
    pub filter: FilterId,
    pub timestamp: Timestamp,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ListContent {
    #[serde(rename = "normal")]
    Normal { content: Digest, full: bool },
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListMeta {
    pub id: ApiResponseId,
    pub content: ListContent,
    pub api_version: ApiVersion,
    pub timestamp: Timestamp,
}
