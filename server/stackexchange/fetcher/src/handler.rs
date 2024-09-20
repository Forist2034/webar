use std::{fmt, marker::PhantomData};

use reqwest::Url;

use webar_stackexchange_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest, ResponseData},
    id::{AnswerId, BadgeId, CollectiveSlug, CommentId, QuestionId, RevisionId, TagName, UserId},
    rest_api::{
        filter::{FilterId, FilterSpec, TypeMap},
        model::{self, ApiObject},
        source::{
            AnswerEdge, Archive, ArchiveChildEdge, ArchiveEdge, ArchiveNode, CollectiveEdge,
            EdgeBranch, EdgeChild, EdgeLeaf, NodeChild, NodeLeaf, QuestionEdge, SiteChildEdge,
            SiteChildNode, SiteEdge, TagEdge, UserEdge,
        },
        ApiInfo, ApiObjectType, ApiVersion,
    },
    KnownSite,
};

pub(crate) const API_VERSION: ApiVersion = ApiVersion::V2_3;
const PAGE_SIZE: &str = "100";

pub struct NodeReq<'s, O> {
    pub(crate) ty: ArchiveNode<&'s str>,
    pub(crate) url: reqwest::Url,
    pub(crate) filter: FilterId,
    _phantom: PhantomData<fn() -> O>,
}
pub struct EdgeReq<'s, O> {
    pub(crate) ty: ArchiveEdge<&'s str>,
    pub(crate) base_url: reqwest::Url,
    filter: FilterId,
    pub(crate) responses: Vec<HttpRequest>,
    _phantom: PhantomData<fn() -> O>,
}
impl<'s, O> EdgeReq<'s, O> {
    pub fn finish(self, seq: u32, full: bool) -> ApiResponse<&'s str, HttpRequest> {
        ApiResponse {
            api: ApiInfo {
                version: API_VERSION,
                filter: self.filter,
            },
            seq,
            data: ResponseData::Edge {
                ty: self.ty,
                full,
                responses: self.responses,
            },
        }
    }
}
pub struct RevisionReq<'s> {
    pub(crate) ty: Archive<&'s str>,
    pub(crate) base_url: reqwest::Url,
    pub(crate) filter: FilterId,
}

pub struct Handler<FN> {
    pub site: KnownSite,
    pub filters: TypeMap<FilterSpec<FN>>,
}
pub struct AnswerHandler<'h, FN> {
    handler: &'h Handler<FN>,
    id: AnswerId,
}
pub struct CollectiveHandler<'h, 's, FN> {
    handler: &'h Handler<FN>,
    slug: CollectiveSlug<&'s str>,
}
pub struct QuestionHandler<'h, FN> {
    handler: &'h Handler<FN>,
    id: QuestionId,
}
pub struct TagHandler<'h, 's, FN> {
    handler: &'h Handler<FN>,
    name: TagName<&'s str>,
}
pub struct UserHandler<'h, FN> {
    handler: &'h Handler<FN>,
    id: UserId,
}

impl<FN: AsRef<str>> Handler<FN> {
    fn node_req<'s, O: ApiObject>(
        &self,
        ty: SiteChildNode<&'s str>,
        path: fmt::Arguments,
    ) -> NodeReq<'s, O> {
        let filter = &self.filters[O::TYPE];
        NodeReq {
            ty: ArchiveNode::Site(self.site, NodeChild::Child(ty)),
            url: Url::parse_with_params(
                format!("https://api.stackexchange.com/2.3{path}").as_str(),
                [
                    ("site", self.site.as_str()),
                    ("filter", filter.name.as_ref()),
                ],
            )
            .unwrap(),
            filter: filter.id,
            _phantom: PhantomData,
        }
    }
    fn format_page_url(&self, path: fmt::Arguments, filter: &str) -> Url {
        Url::parse_with_params(
            format!("https://api.stackexchange.com/2.3{path}").as_str(),
            [
                ("site", self.site.as_str()),
                ("filter", filter),
                ("pagesize", PAGE_SIZE),
            ],
        )
        .unwrap()
    }
    fn edge_req<'s, O: ApiObject>(
        &self,
        ty: EdgeBranch<SiteEdge, SiteChildEdge<&'s str>>,
        path: fmt::Arguments,
    ) -> EdgeReq<'s, O> {
        let filter = &self.filters[O::TYPE];
        EdgeReq {
            ty: ArchiveEdge(EdgeChild::Child(ArchiveChildEdge::Site(self.site, ty))),
            base_url: self.format_page_url(path, filter.name.as_ref()),
            filter: filter.id,
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
    fn revision_req<'s>(&self, ty: Archive<&'s str>, path: fmt::Arguments) -> RevisionReq<'s> {
        let filter = &self.filters[ApiObjectType::Revision];
        RevisionReq {
            ty,
            base_url: self.format_page_url(path, filter.name.as_ref()),
            filter: filter.id,
        }
    }
    fn site_child_revision_req<'s>(
        &self,
        ty: SiteChildEdge<&'s str>,
        path: fmt::Arguments,
    ) -> RevisionReq<'s> {
        self.revision_req(
            Archive::Edge(ArchiveEdge(EdgeChild::Child(ArchiveChildEdge::Site(
                self.site,
                EdgeBranch::Child(ty),
            )))),
            path,
        )
    }

    pub fn answer<'h>(&'h self, id: AnswerId) -> AnswerHandler<'h, FN> {
        AnswerHandler { handler: self, id }
    }

    pub fn get_badge(&self, id: BadgeId) -> NodeReq<'static, model::Badge> {
        self.node_req(
            SiteChildNode::Badge(id, NodeLeaf::Node),
            format_args!("/badges/{id}"),
        )
    }
    pub fn list_badges(&self) -> EdgeReq<'static, model::Badge> {
        self.edge_req(EdgeBranch::Edge(SiteEdge::Badge), format_args!("/badges"))
    }

    pub fn collective<'h, 's>(
        &'h self,
        slug: CollectiveSlug<&'s str>,
    ) -> CollectiveHandler<'h, 's, FN> {
        CollectiveHandler {
            handler: self,
            slug,
        }
    }

    pub fn get_comment(&self, id: CommentId) -> NodeReq<'static, model::Comment> {
        self.node_req(
            SiteChildNode::Comment(id, NodeLeaf::Node),
            format_args!("/comments/{id}"),
        )
    }

    pub fn get_info(&self) -> NodeReq<'static, model::Info> {
        self.node_req(SiteChildNode::Info(NodeLeaf::Node), format_args!("/info"))
    }

    pub fn question<'h>(&'h self, id: QuestionId) -> QuestionHandler<'h, FN> {
        QuestionHandler { handler: self, id }
    }

    pub fn get_revision(&self, id: RevisionId) -> RevisionReq<'static> {
        self.revision_req(
            Archive::Node(ArchiveNode::Site(
                self.site,
                NodeChild::Child(SiteChildNode::Revision(id, NodeLeaf::Node)),
            )),
            format_args!("/revisions/{id}"),
        )
    }

    pub fn tag<'h, 's>(&'h self, name: TagName<&'s str>) -> TagHandler<'h, 's, FN> {
        TagHandler {
            handler: self,
            name,
        }
    }
    pub fn list_tags(&self) -> EdgeReq<'static, model::Tag> {
        self.edge_req(EdgeBranch::Edge(SiteEdge::Tag), format_args!("/tags"))
    }

    pub fn user<'h>(&'h self, id: UserId) -> UserHandler<'h, FN> {
        UserHandler { handler: self, id }
    }
}
impl<'h, FN: AsRef<str>> AnswerHandler<'h, FN> {
    pub fn get(&self) -> NodeReq<'static, model::Answer> {
        self.handler.node_req(
            SiteChildNode::Answer(self.id, NodeLeaf::Node),
            format_args!("/answers/{}", self.id),
        )
    }
    pub fn list_comments(&self) -> EdgeReq<'static, model::Comment> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::Answer(
                self.id,
                EdgeLeaf::Edge(AnswerEdge::Comment),
            )),
            format_args!("/answers/{}/comments", self.id),
        )
    }
    pub fn list_revisions(&self) -> RevisionReq<'static> {
        self.handler.site_child_revision_req(
            SiteChildEdge::Answer(self.id, EdgeLeaf::Edge(AnswerEdge::Revision)),
            format_args!("/posts/{}/revisions", self.id),
        )
    }
}
impl<'h, 's, FN: AsRef<str>> CollectiveHandler<'h, 's, FN> {
    pub fn get(&self) -> NodeReq<'s, model::Collective> {
        self.handler.node_req(
            SiteChildNode::Collective(self.slug, NodeLeaf::Node),
            format_args!("/collectives/{}", self.slug),
        )
    }

    fn edge_req<O: ApiObject>(&self, ty: CollectiveEdge, path: fmt::Arguments) -> EdgeReq<'s, O> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::Collective(self.slug, EdgeLeaf::Edge(ty))),
            path,
        )
    }
    pub fn answers(&self) -> EdgeReq<'s, model::Answer> {
        self.edge_req(
            CollectiveEdge::Answer,
            format_args!("/collectives/{}/answers", self.slug),
        )
    }
    pub fn questions(&self) -> EdgeReq<'s, model::Question> {
        self.edge_req(
            CollectiveEdge::Question,
            format_args!("/collectives/{}/questions", self.slug),
        )
    }
    pub fn tags(&self) -> EdgeReq<'s, model::Tag> {
        self.edge_req(
            CollectiveEdge::Tag,
            format_args!("/collectives/{}/tags", self.slug),
        )
    }
    pub fn users(&self) -> EdgeReq<'s, model::User> {
        self.edge_req(
            CollectiveEdge::User,
            format_args!("/collectives/{}/users", self.slug),
        )
    }
}
impl<'h, FN: AsRef<str>> QuestionHandler<'h, FN> {
    pub fn get(&self) -> NodeReq<'static, model::Question> {
        self.handler.node_req(
            SiteChildNode::Question(self.id, NodeLeaf::Node),
            format_args!("/questions/{}", self.id),
        )
    }
    pub fn list_answers(&self) -> EdgeReq<'static, model::Answer> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::Question(
                self.id,
                EdgeLeaf::Edge(QuestionEdge::Answer),
            )),
            format_args!("/questions/{}/answers", self.id),
        )
    }
    pub fn list_comments(&self) -> EdgeReq<'static, model::Comment> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::Question(
                self.id,
                EdgeLeaf::Edge(QuestionEdge::Answer),
            )),
            format_args!("/questions/{}/comments", self.id),
        )
    }
    pub fn list_revisions(&self) -> RevisionReq<'static> {
        self.handler.site_child_revision_req(
            SiteChildEdge::Question(self.id, EdgeLeaf::Edge(QuestionEdge::Revision)),
            format_args!("/posts/{}/revisions", self.id),
        )
    }
}
impl<'h, 's, FN: AsRef<str>> TagHandler<'h, 's, FN> {
    pub fn get(&self) -> NodeReq<'s, model::Tag> {
        self.handler.node_req(
            SiteChildNode::Tag(self.name, NodeLeaf::Node),
            format_args!("/tags/{}/info", self.name),
        )
    }
    pub fn get_wiki(&self) -> NodeReq<'s, model::TagWiki> {
        self.handler.node_req(
            SiteChildNode::TagWiki(self.name, NodeLeaf::Node),
            format_args!("/tags/{}/wikis", self.name),
        )
    }
    pub fn list_synonyms(&self) -> EdgeReq<'s, model::TagSynonym> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::Tag(
                self.name,
                EdgeLeaf::Edge(TagEdge::TagSynonym),
            )),
            format_args!("/tags/{}/synonyms", self.name),
        )
    }
}
impl<'h, FN: AsRef<str>> UserHandler<'h, FN> {
    pub fn get(&self) -> NodeReq<'static, model::User> {
        self.handler.node_req(
            SiteChildNode::User(self.id, NodeLeaf::Node),
            format_args!("/users/{}", self.id),
        )
    }

    fn edge_req<O: ApiObject>(&self, ty: UserEdge, path: fmt::Arguments) -> EdgeReq<'static, O> {
        self.handler.edge_req(
            EdgeBranch::Child(SiteChildEdge::User(self.id, EdgeLeaf::Edge(ty))),
            path,
        )
    }
    pub fn list_answers(&self) -> EdgeReq<'static, model::Answer> {
        self.edge_req(UserEdge::Answer, format_args!("/users/{}/answers", self.id))
    }
    pub fn list_badges(&self) -> EdgeReq<'static, model::Badge> {
        self.edge_req(UserEdge::Badge, format_args!("/users/{}/badges", self.id))
    }
    pub fn list_comments(&self) -> EdgeReq<'static, model::Comment> {
        self.edge_req(
            UserEdge::Comment,
            format_args!("/users/{}/comments", self.id),
        )
    }
    pub fn list_questions(&self) -> EdgeReq<'static, model::Question> {
        self.edge_req(
            UserEdge::Question,
            format_args!("/users/{}/questions", self.id),
        )
    }
}
