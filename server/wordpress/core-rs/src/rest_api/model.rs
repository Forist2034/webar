use serde::Deserialize;

use crate::id::{
    CategoryId, CommentId, MediaId, PageId, PageRevisionId, PostId, PostRevisionId, TagId, UserId,
};

#[derive(Debug, Deserialize)]
pub struct Category {
    pub id: CategoryId,
}

#[derive(Debug, Deserialize)]
pub struct Comment {
    pub id: CommentId,
    pub author: UserId,
    pub post: PostId,
}

#[derive(Debug, Deserialize)]
pub struct Media {
    pub id: MediaId,
    pub post: Option<PostId>,
}

#[derive(Debug, Deserialize)]
pub struct Page {
    pub id: PageId,
    pub author: UserId,
    pub featured_media: Option<MediaId>,
}

#[derive(Debug, Deserialize)]
pub struct PageRevision {
    pub id: PageRevisionId,
    pub author: UserId,
    pub parent: u64,
}

#[derive(Debug, Deserialize)]
pub struct Post {
    pub id: PostId,
    pub author: UserId,
    pub categories: Vec<CategoryId>,
    pub tags: Vec<TagId>,
}

#[derive(Debug, Deserialize)]
pub struct PostRevision {
    pub author: UserId,
    pub id: PostRevisionId,
    pub parent: PostId,
}

#[derive(Debug, Deserialize)]
pub struct Tag {
    pub id: TagId,
}

#[derive(Debug, Deserialize)]
pub struct User {
    pub id: UserId,
}
