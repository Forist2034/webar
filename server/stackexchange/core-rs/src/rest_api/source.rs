use serde::Deserialize;
use uuid::Uuid;

use webar_core::http;
use webar_data::ser::Serialize;

use crate::{
    id::{AnswerId, BadgeId, CollectiveSlug, CommentId, QuestionId, RevisionId, TagName, UserId},
    KnownSite,
};

macro_rules! archive {
    (enum $i:ident {$($r:ident = $v:literal,)+}) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        pub enum $i {
            $(#[serde(rename=$v)] $r,)+
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NodeLeaf {
    #[serde(rename = "node")]
    Node,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NodeChild<C> {
    #[serde(rename = "child")]
    Child(C),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SiteChildNode<S> {
    #[serde(rename = "answer")]
    Answer(AnswerId, NodeLeaf),
    #[serde(rename = "badge")]
    Badge(BadgeId, NodeLeaf),
    #[serde(rename = "comment")]
    Comment(CommentId, NodeLeaf),
    #[serde(rename = "collective")]
    Collective(CollectiveSlug<S>, NodeLeaf),
    #[serde(rename = "info")]
    Info(NodeLeaf),
    #[serde(rename = "question")]
    Question(QuestionId, NodeLeaf),
    #[serde(rename = "revision")]
    Revision(RevisionId, NodeLeaf),
    #[serde(rename = "tag")]
    Tag(TagName<S>, NodeLeaf),
    #[serde(rename = "tag_wiki")]
    TagWiki(TagName<S>, NodeLeaf),
    #[serde(rename = "tag_synonym")]
    TagSynonym(TagName<S>, NodeLeaf),
    #[serde(rename = "user")]
    User(UserId, NodeLeaf),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveNode<S> {
    #[serde(rename = "site")]
    Site(KnownSite, NodeChild<SiteChildNode<S>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeLeaf<E> {
    #[serde(rename = "edge")]
    Edge(E),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeChild<C> {
    #[serde(rename = "child")]
    Child(C),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeBranch<E, C> {
    #[serde(rename = "edge")]
    Edge(E),
    #[serde(rename = "child")]
    Child(C),
}

archive!(
    enum AnswerEdge {
        Comment = "comment",
        Revision = "revision",
    }
);
archive!(
    enum CollectiveEdge {
        Answer = "answer",
        Question = "question",
        Tag = "tag",
        User = "user",
    }
);
archive!(
    enum QuestionEdge {
        Answer = "answer",
        Comment = "comment",
        Revision = "revision",
    }
);
archive!(
    enum TagEdge {
        TagSynonym = "tag_synonym",
    }
);
archive!(
    enum UserEdge {
        Answer = "answer",
        Badge = "badge",
        Comment = "comment",
        Question = "question",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SiteChildEdge<S> {
    #[serde(rename = "answer")]
    Answer(AnswerId, EdgeLeaf<AnswerEdge>),
    #[serde(rename = "collective")]
    Collective(CollectiveSlug<S>, EdgeLeaf<CollectiveEdge>),
    #[serde(rename = "question")]
    Question(QuestionId, EdgeLeaf<QuestionEdge>),
    #[serde(rename = "tag")]
    Tag(TagName<S>, EdgeLeaf<TagEdge>),
    #[serde(rename = "user")]
    User(UserId, EdgeLeaf<UserEdge>),
}
archive!(
    enum SiteEdge {
        Badge = "badge",
        Tag = "tag",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveChildEdge<S> {
    #[serde(rename = "site")]
    Site(KnownSite, EdgeBranch<SiteEdge, SiteChildEdge<S>>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ArchiveEdge<S>(pub EdgeChild<ArchiveChildEdge<S>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Archive<S> {
    #[serde(rename = "node")]
    Node(ArchiveNode<S>),
    #[serde(rename = "edge")]
    Edge(ArchiveEdge<S>),
}

pub use http::RequestId;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ResponseId {
    #[serde(rename = "x-request-guid")]
    /// `x-request-guid` header
    XRequestGuid(Uuid),
}

pub type Request<U> = http::Request<RequestId, U, (), ()>;
pub type Response<B> = http::Response<Option<ResponseId>, B>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ResponseData<S, R> {
    #[serde(rename = "node")]
    Node {
        #[serde(rename = "type")]
        ty: ArchiveNode<S>,
        response: R,
    },
    #[serde(rename = "edge")]
    Edge {
        #[serde(rename = "type")]
        ty: ArchiveEdge<S>,
        full: bool,
        responses: Vec<R>,
    },
    /** A revision id may correspond to multiple revision objects.
     Revision nodes and edges must be fully retrieved to avoid getting
     partial revision.
    */
    #[serde(rename = "revision")]
    Revision {
        #[serde(rename = "type")]
        ty: Archive<S>,
        responses: Vec<R>,
    },
}
