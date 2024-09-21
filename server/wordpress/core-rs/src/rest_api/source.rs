use serde::Deserialize;

use webar_core::http;
use webar_data::ser::Serialize;

use crate::id::{CategoryId, CommentId, MediaId, PageId, PostId, TagId, UserId};

macro_rules! archive {
    (enum $i:ident { $($r:ident = $v:literal,)+ }) => {
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
pub enum BlogChildNode {
    #[serde(rename = "category")]
    Category(CategoryId, NodeLeaf),
    #[serde(rename = "comment")]
    Comment(CommentId, NodeLeaf),
    #[serde(rename = "media")]
    Media(MediaId, NodeLeaf),
    #[serde(rename = "page")]
    Page(PageId, NodeLeaf),
    #[serde(rename = "post")]
    Post(PostId, NodeLeaf),
    #[serde(rename = "tag")]
    Tag(TagId, NodeLeaf),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveNode<Addr> {
    #[serde(rename = "blog")]
    Blog(Addr, NodeChild<BlogChildNode>),
    #[serde(rename = "user")]
    User(UserId, NodeLeaf),
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
    enum PostEdge {
        Comment = "comment",
    }
);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlogChildEdge {
    #[serde(rename = "post")]
    Post(PostId, EdgeLeaf<PostEdge>),
}
archive!(
    enum BlogEdge {
        Category = "category",
        Comment = "comment",
        Media = "media",
        Page = "page",
        Post = "post",
        Tag = "tag",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveChildEdge<Addr> {
    #[serde(rename = "blog")]
    Blog(Addr, EdgeBranch<BlogEdge, BlogChildEdge>),
}
archive!(
    enum ArchiveSelfEdge {
        User = "user",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ArchiveEdge<Addr>(pub EdgeBranch<ArchiveSelfEdge, ArchiveChildEdge<Addr>>);

pub use http::RequestId;

pub type Request<U> = http::Request<RequestId, U, ()>;
pub type Response<B> = http::Response<(), http::HeaderMap<http::HeaderValue>, B>;

#[derive(Debug, Serialize, Deserialize)]
pub enum ResponseData<Addr, R> {
    #[serde(rename = "node")]
    Node {
        #[serde(rename = "type")]
        ty: ArchiveNode<Addr>,
        response: R,
    },
    #[serde(rename = "edge")]
    Edge {
        #[serde(rename = "type")]
        ty: ArchiveEdge<Addr>,
        full: bool,
        responses: Vec<R>,
    },
}
