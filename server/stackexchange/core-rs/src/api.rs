use std::fmt::Display;

use serde::Deserialize;

use crate::{
    id::{
        AccountId, AnswerId, BadgeId, CollectiveSlug, CommentId, QuestionId, RevisionId, TagName,
        UserId,
    },
    ObjectType,
};

pub trait Object: serde::de::DeserializeOwned {
    const TYPE: ObjectType;
    type Id<'a>: Display
    where
        Self: 'a;
    type OwnedId: Display;
    fn id(&self) -> Self::Id<'_>;
    fn into_id(self) -> Self::OwnedId;
}

#[derive(Deserialize)]
pub struct ShallowUser {
    #[serde(default)]
    pub account_id: Option<AccountId>,
    #[serde(default)]
    pub user_id: Option<UserId>,
}

#[derive(Deserialize)]
pub struct CollectiveRef {
    pub slug: CollectiveSlug<String>,
}

#[derive(Deserialize)]
pub struct Answer {
    pub answer_id: AnswerId,
    pub question_id: QuestionId,
    #[serde(default)]
    pub collectives: Vec<CollectiveRef>,
    pub tags: Vec<TagName<String>>,
    #[serde(default)]
    pub last_editor: Option<ShallowUser>,
    #[serde(default)]
    pub owner: Option<ShallowUser>,
}
impl Object for Answer {
    const TYPE: ObjectType = ObjectType::Answer;
    type Id<'a> = AnswerId;
    type OwnedId = AnswerId;
    fn id(&self) -> Self::Id<'_> {
        self.answer_id
    }
    fn into_id(self) -> Self::OwnedId {
        self.answer_id
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PostType {
    Question,
    Answer,
    Article,
}
#[derive(Deserialize)]
pub struct Comment {
    pub comment_id: CommentId,
    pub post_id: u64,
    pub post_type: PostType,
    #[serde(default)]
    pub owner: Option<ShallowUser>,
}
impl Object for Comment {
    const TYPE: ObjectType = ObjectType::Comment;
    type Id<'a> = CommentId;
    type OwnedId = CommentId;
    fn id(&self) -> Self::Id<'_> {
        self.comment_id
    }
    fn into_id(self) -> Self::OwnedId {
        self.comment_id
    }
}

#[derive(Deserialize)]
pub struct Badge {
    pub badge_id: BadgeId,
}
impl Object for Badge {
    const TYPE: ObjectType = ObjectType::Badge;
    type Id<'a> = BadgeId;
    type OwnedId = BadgeId;
    fn id(&self) -> Self::Id<'_> {
        self.badge_id
    }
    fn into_id(self) -> Self::OwnedId {
        self.badge_id
    }
}

#[derive(Deserialize)]
pub struct Collective {
    pub slug: CollectiveSlug<String>,
    pub tags: Vec<TagName<String>>,
}
impl Object for Collective {
    const TYPE: ObjectType = ObjectType::Collective;
    type Id<'a> = CollectiveSlug<&'a str>;
    type OwnedId = CollectiveSlug<String>;
    fn id(&self) -> Self::Id<'_> {
        CollectiveSlug(&self.slug.0)
    }
    fn into_id(self) -> Self::OwnedId {
        self.slug
    }
}

#[derive(Deserialize)]
pub struct Revision {
    pub revision_guid: RevisionId,
    pub post_id: u64,
    pub post_type: PostType,
}
impl Object for Revision {
    const TYPE: ObjectType = ObjectType::Revision;
    type Id<'a> = RevisionId;
    type OwnedId = RevisionId;
    fn id(&self) -> Self::Id<'_> {
        self.revision_guid
    }
    fn into_id(self) -> Self::OwnedId {
        self.revision_guid
    }
}

#[derive(Deserialize)]
pub struct Tag {
    #[serde(default)]
    pub collectives: Vec<CollectiveRef>,
    pub name: TagName<String>,
    #[serde(default)]
    pub synonyms: Vec<TagName<String>>,
}
impl Object for Tag {
    const TYPE: ObjectType = ObjectType::Tag;
    type Id<'a> = TagName<&'a str>;
    type OwnedId = TagName<String>;
    fn id(&self) -> Self::Id<'_> {
        TagName(&self.name.0)
    }
    fn into_id(self) -> Self::OwnedId {
        self.name
    }
}

#[derive(Deserialize)]
pub struct TagSynonym {
    pub from_tag: TagName<String>,
    pub to_tag: TagName<String>,
}
impl Object for TagSynonym {
    const TYPE: ObjectType = ObjectType::TagSynonym;
    type Id<'a> = TagName<&'a str>;
    type OwnedId = TagName<String>;
    fn id(&self) -> Self::Id<'_> {
        TagName(&self.from_tag.0)
    }
    fn into_id(self) -> Self::OwnedId {
        self.from_tag
    }
}

#[derive(Deserialize)]
pub struct TagWiki {
    #[serde(default)]
    pub last_body_editor: Option<ShallowUser>,
    #[serde(default)]
    pub last_excerpt_editor: Option<ShallowUser>,
    pub tag_name: TagName<String>,
}
impl Object for TagWiki {
    const TYPE: ObjectType = ObjectType::TagWiki;
    type Id<'a> = TagName<&'a str>;
    type OwnedId = TagName<String>;
    fn id(&self) -> Self::Id<'_> {
        TagName(&self.tag_name.0)
    }
    fn into_id(self) -> Self::OwnedId {
        self.tag_name
    }
}

#[derive(Deserialize)]
pub struct CollectiveMembership {
    pub collective: CollectiveRef,
}

#[derive(Deserialize)]
pub struct User {
    pub account_id: AccountId,
    #[serde(default)]
    pub collectives: Vec<CollectiveMembership>,
    pub user_id: UserId,
}
impl Object for User {
    const TYPE: ObjectType = ObjectType::User;
    type Id<'a> = UserId;
    type OwnedId = UserId;
    fn id(&self) -> Self::Id<'_> {
        self.user_id
    }
    fn into_id(self) -> Self::OwnedId {
        self.user_id
    }
}

#[derive(Deserialize)]
pub struct Question {
    pub question_id: QuestionId,
    #[serde(default)]
    pub collectives: Vec<CollectiveRef>,
    pub tags: Vec<TagName<String>>,
    #[serde(default)]
    pub last_editor: Option<ShallowUser>,
    #[serde(default)]
    pub owner: Option<ShallowUser>,
}
impl Object for Question {
    const TYPE: ObjectType = ObjectType::Question;
    type Id<'a> = QuestionId;
    type OwnedId = QuestionId;
    fn id(&self) -> Self::Id<'_> {
        self.question_id
    }
    fn into_id(self) -> Self::OwnedId {
        self.question_id
    }
}

#[derive(Deserialize)]
pub struct Info<S> {
    pub site: S,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum FilterType {
    Safe,
    Unsafe,
    Invalid,
}

#[derive(Deserialize)]
pub struct Filter<N> {
    pub filter: N,
    pub filter_type: FilterType,
    pub included_fields: Vec<N>,
}

#[derive(Deserialize)]
pub struct Wrapper<IS> {
    #[serde(default)]
    pub backoff: Option<u64>,
    #[serde(default)]
    pub error_id: Option<u64>,
    #[serde(default)]
    pub error_message: Option<String>,
    #[serde(default)]
    pub error_name: Option<String>,
    #[serde(default)]
    pub has_more: bool,
    pub quota_remaining: u64,
    pub items: IS,
}
