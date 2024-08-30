use serde::Deserialize;

use webar_core::http;
use webar_data::ser::Serialize;

use crate::id::{PageId, PostId};

macro_rules! archive {
    (enum $i:ident { $($r:ident = $v:literal,)+ }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        pub enum $i {
            $(#[serde(rename=$v)] $r,)+
        }
    };
}

archive!(
    enum ArchivePageEdge {
        Revision = "revision",
    }
);
archive!(
    enum ArchivePostEdge {
        Comment = "comment",
        Revision = "revision",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveBlogEdge {
    #[serde(rename = "page")]
    Page(PageId, ArchivePageEdge),
    #[serde(rename = "post")]
    Post(PostId, ArchivePostEdge),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveEdge<Addr> {
    #[serde(rename = "blog")]
    Blog(Addr, ArchiveBlogEdge),
}

archive!(
    enum ArchiveBlogCollection {
        Category = "category",
        Comment = "comment",
        Media = "media",
        Page = "page",
        Post = "post",
        Tag = "tag",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArchiveCollection<Addr> {
    #[serde(rename = "blog")]
    Blog(Addr, ArchiveBlogCollection),
    #[serde(rename = "user")]
    User,
}

pub use http::RequestId;

pub type Request<U> = http::Request<RequestId, U, ()>;
pub type Response<B> = http::Response<(), http::HeaderMap<http::HeaderValue>, B>;

archive!(
    enum NodeTypeBlog {
        Category = "category",
        Comment = "comment",
        Media = "media",
        Page = "page",
        PageRevision = "page_revision",
        Post = "post",
        PostRevision = "post_revision",
        Tag = "tag",
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NodeType<Addr> {
    #[serde(rename = "blog")]
    Blog(Addr, NodeTypeBlog),
    #[serde(rename = "user")]
    User,
}
#[derive(Debug, Serialize, Deserialize)]
pub enum ResponseData<Addr, R> {
    #[serde(rename = "node")]
    Node {
        #[serde(rename = "type")]
        ty: NodeType<Addr>,
        response: R,
    },
    #[serde(rename = "edge")]
    Edge {
        #[serde(rename = "type")]
        ty: ArchiveEdge<Addr>,
        full: bool,
        responses: Vec<R>,
    },
    #[serde(rename = "collection")]
    Collection {
        #[serde(rename = "type")]
        ty: ArchiveCollection<Addr>,
        full: bool,
        responses: Vec<R>,
    },
}
