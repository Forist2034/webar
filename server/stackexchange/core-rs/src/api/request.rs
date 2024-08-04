use std::fmt::Display;

use serde::Deserialize;
use uuid::Uuid;

use webar_data::ser::Serialize;

use crate::id;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ApiObjectType {
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
impl ApiObjectType {
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
impl Display for ApiObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
impl Serialize for ApiObjectType {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct List<LR, R> {
    pub request: LR,
    pub full: bool,
    pub responses: Vec<R>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Objects<R> {
    #[serde(rename = "type")]
    pub ty: ApiObjectType,
    pub response: R,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResponseData<O, L> {
    #[serde(rename = "objects")]
    Objects(O),
    #[serde(rename = "list")]
    List(L),
}
