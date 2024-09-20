use serde::Deserialize;

use crate::{
    id::{
        AccountId, AnswerId, BadgeId, CollectiveSlug, CommentId, QuestionId, RevisionId, TagName,
        UserId,
    },
    rest_api::ApiObjectType,
};

pub trait ApiObject: serde::de::DeserializeOwned {
    const TYPE: ApiObjectType;
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
    #[serde(default)]
    pub tags: Vec<TagName<String>>,
    #[serde(default)]
    pub last_editor: Option<ShallowUser>,
    #[serde(default)]
    pub owner: Option<ShallowUser>,
}
impl ApiObject for Answer {
    const TYPE: ApiObjectType = ApiObjectType::Answer;
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
impl ApiObject for Comment {
    const TYPE: ApiObjectType = ApiObjectType::Comment;
}

#[derive(Deserialize)]
pub struct Badge {
    pub badge_id: BadgeId,
}
impl ApiObject for Badge {
    const TYPE: ApiObjectType = ApiObjectType::Badge;
}

#[derive(Deserialize)]
pub struct Collective {
    pub slug: CollectiveSlug<String>,
    #[serde(default)]
    pub tags: Vec<TagName<String>>,
}
impl ApiObject for Collective {
    const TYPE: ApiObjectType = ApiObjectType::Collective;
}

#[derive(Deserialize)]
pub struct Revision {
    pub revision_guid: RevisionId,
    pub post_id: u64,
    pub post_type: PostType,
}
impl ApiObject for Revision {
    const TYPE: ApiObjectType = ApiObjectType::Revision;
}

#[derive(Deserialize)]
pub struct Tag {
    #[serde(default)]
    pub collectives: Vec<CollectiveRef>,
    pub name: TagName<String>,
    #[serde(default)]
    pub synonyms: Vec<TagName<String>>,
}
impl ApiObject for Tag {
    const TYPE: ApiObjectType = ApiObjectType::Tag;
}

#[derive(Deserialize)]
pub struct TagSynonym {
    pub from_tag: TagName<String>,
    pub to_tag: TagName<String>,
}
impl ApiObject for TagSynonym {
    const TYPE: ApiObjectType = ApiObjectType::TagSynonym;
}

#[derive(Deserialize)]
pub struct TagWiki {
    #[serde(default)]
    pub last_body_editor: Option<ShallowUser>,
    #[serde(default)]
    pub last_excerpt_editor: Option<ShallowUser>,
    pub tag_name: TagName<String>,
}
impl ApiObject for TagWiki {
    const TYPE: ApiObjectType = ApiObjectType::TagWiki;
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
impl ApiObject for User {
    const TYPE: ApiObjectType = ApiObjectType::User;
}

#[derive(Deserialize)]
pub struct Question {
    pub question_id: QuestionId,
    #[serde(default)]
    pub collectives: Vec<CollectiveRef>,
    #[serde(default)]
    pub tags: Vec<TagName<String>>,
    #[serde(default)]
    pub last_editor: Option<ShallowUser>,
    #[serde(default)]
    pub owner: Option<ShallowUser>,
}
impl ApiObject for Question {
    const TYPE: ApiObjectType = ApiObjectType::Question;
}

#[derive(Deserialize)]
pub struct Info {}
impl ApiObject for Info {
    const TYPE: ApiObjectType = ApiObjectType::Info;
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
