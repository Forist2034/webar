use std::{fmt, marker::PhantomData};

use reqwest::Url;

use webar_wordpress_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest},
    id::{CategoryId, CommentId, MediaId, PageId, PostId, TagId, UserId},
    rest_api::{
        model,
        source::{
            self, ArchiveChildEdge, ArchiveEdge, ArchiveNode, ArchiveSelfEdge, BlogChildEdge,
            BlogChildNode, BlogEdge, EdgeBranch, EdgeLeaf, NodeChild, NodeLeaf, PostEdge,
        },
        ApiInfo, ApiVersion,
    },
    Address,
};

pub(crate) const CURRENT_API: ApiVersion = ApiVersion::V2;
const PER_PAGE_STR: &str = "100";

enum ApiBaseInner<'s> {
    Param(&'s str),
    Path {
        https: bool,
        domain: &'s str,
        prefix: &'s str,
    },
    WordpressCom(&'s str),
}
impl<'s> ApiBaseInner<'s> {
    fn format_url(&self, route: impl fmt::Display) -> Url {
        match self {
            Self::Param(b) => {
                Url::parse_with_params(*b, &[("rest_route", format!("/wp/v2{route}"))]).unwrap()
            }
            Self::Path {
                https,
                domain,
                prefix,
            } => Url::parse(&format!(
                "{}://{domain}{prefix}/wp-json/wp/v2{route}",
                if *https { "https" } else { "http" }
            ))
            .unwrap(),
            Self::WordpressCom(domain) => Url::parse(&format!(
                "https://public-api.wordpress.com/wp/v2/sites/{domain}{route}"
            ))
            .unwrap(),
        }
    }
}

pub struct ApiBase<'s>(ApiBaseInner<'s>);
impl<'s> ApiBase<'s> {
    pub fn param(base_url: &'s str) -> Self {
        Self(ApiBaseInner::Param(base_url))
    }
    pub fn path(https: bool, domain: &'s str, prefix: &'s str) -> Self {
        Self(ApiBaseInner::Path {
            https,
            domain,
            prefix,
        })
    }
    pub fn wordpress_com(name: &'s str) -> Self {
        Self(ApiBaseInner::WordpressCom(name))
    }

    #[inline]
    fn format_url(&self, route: impl fmt::Display) -> Url {
        self.0.format_url(route)
    }
    fn edge_url_with_param(&self, route: impl fmt::Display, params: &[(&str, &str)]) -> Url {
        let mut ret = self.format_url(route);
        ret.query_pairs_mut()
            .append_pair("per_page", PER_PAGE_STR)
            .extend_pairs(params);
        ret
    }
}

pub type StrAddr<'s> = Address<&'s str, &'s str>;

pub struct NodeReq<'s, 'a, O> {
    pub(crate) ty: ArchiveNode<&'a StrAddr<'s>>,
    pub(crate) url: Url,
    _phantom: PhantomData<fn() -> O>,
}
pub struct EdgeReq<'s, 'a, O> {
    pub(crate) ty: ArchiveEdge<&'a StrAddr<'s>>,
    pub(crate) base_url: Url,
    pub(crate) responses: Vec<HttpRequest>,
    _phantom: PhantomData<fn() -> O>,
}
impl<'a, 's, O> EdgeReq<'s, 'a, O> {
    pub fn finish(self, seq: u32, full: bool) -> ApiResponse<&'a StrAddr<'s>, HttpRequest> {
        ApiResponse {
            api: ApiInfo {
                version: CURRENT_API,
            },
            seq,
            data: source::ResponseData::Edge {
                ty: self.ty,
                full,
                responses: self.responses,
            },
        }
    }
}

pub struct Handler<'s> {
    pub blog: StrAddr<'s>,
    pub api_base: ApiBase<'s>,
}
pub struct PostHandler<'s, 'h> {
    handler: &'h Handler<'s>,
    id: PostId,
}
impl<'s> Handler<'s> {
    fn blog_node_req<'h, O>(
        &'h self,
        ty: BlogChildNode,
        route: fmt::Arguments,
    ) -> NodeReq<'s, 'h, O> {
        NodeReq {
            ty: ArchiveNode::Blog(&self.blog, NodeChild::Child(ty)),
            url: self.api_base.format_url(route),
            _phantom: PhantomData,
        }
    }
    fn blog_edge_req<'h, O>(
        &'h self,
        ty: EdgeBranch<BlogEdge, BlogChildEdge>,
        route: impl fmt::Display,
        params: &[(&str, &str)],
    ) -> EdgeReq<'s, 'h, O> {
        EdgeReq {
            ty: ArchiveEdge(EdgeBranch::Child(ArchiveChildEdge::Blog(&self.blog, ty))),
            base_url: self.api_base.edge_url_with_param(route, params),
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
    fn blog_self_edge_req<'h, O>(&'h self, ty: BlogEdge, route: &str) -> EdgeReq<'s, 'h, O> {
        self.blog_edge_req(EdgeBranch::Edge(ty), route, &[])
    }
    fn blog_child_edge_req<'h, O>(
        &'h self,
        ty: BlogChildEdge,
        route: fmt::Arguments,
        params: &[(&str, &str)],
    ) -> EdgeReq<'s, 'h, O> {
        self.blog_edge_req(EdgeBranch::Child(ty), route, params)
    }

    pub fn get_category<'h>(&'h self, id: CategoryId) -> NodeReq<'s, 'h, model::Category> {
        self.blog_node_req(
            BlogChildNode::Category(id, NodeLeaf::Node),
            format_args!("/categories/{id}"),
        )
    }
    pub fn list_categories<'h>(&'h self) -> EdgeReq<'s, 'h, model::Category> {
        self.blog_self_edge_req(BlogEdge::Category, "/categories")
    }

    pub fn get_comment<'h>(&'h self, id: CommentId) -> NodeReq<'s, 'h, model::Comment> {
        self.blog_node_req(
            BlogChildNode::Comment(id, NodeLeaf::Node),
            format_args!("/comments/{id}"),
        )
    }
    pub fn list_comments<'h>(&'h self) -> EdgeReq<'s, 'h, model::Comment> {
        self.blog_self_edge_req(BlogEdge::Comment, "/comments")
    }

    pub fn get_media<'h>(&'h self, id: MediaId) -> NodeReq<'s, 'h, model::Media> {
        self.blog_node_req(
            BlogChildNode::Media(id, NodeLeaf::Node),
            format_args!("/media/{id}"),
        )
    }
    pub fn list_media<'h>(&'h self) -> EdgeReq<'s, 'h, model::Media> {
        self.blog_self_edge_req(BlogEdge::Media, "/media")
    }

    pub fn get_page<'h>(&'h self, id: PageId) -> NodeReq<'s, 'h, model::Page> {
        self.blog_node_req(
            BlogChildNode::Page(id, NodeLeaf::Node),
            format_args!("/pages/{id}",),
        )
    }
    pub fn list_pages<'h>(&'h self) -> EdgeReq<'s, 'h, model::Page> {
        self.blog_self_edge_req(BlogEdge::Page, "/pages")
    }

    pub fn post<'h>(&'h self, id: PostId) -> PostHandler<'s, 'h> {
        PostHandler { handler: self, id }
    }
    pub fn list_posts<'h>(&'h self) -> EdgeReq<'s, 'h, model::Post> {
        self.blog_self_edge_req(BlogEdge::Post, "/posts")
    }

    pub fn get_tag<'h>(&'h self, id: TagId) -> NodeReq<'s, 'h, model::Tag> {
        self.blog_node_req(
            BlogChildNode::Tag(id, NodeLeaf::Node),
            format_args!("/tags/{id}"),
        )
    }
    pub fn list_tags<'h>(&'h self) -> EdgeReq<'s, 'h, model::Tag> {
        self.blog_self_edge_req(BlogEdge::Tag, "/tags")
    }

    pub fn get_user(&self, id: UserId) -> NodeReq<'static, 'static, model::User> {
        NodeReq {
            ty: ArchiveNode::User(id, NodeLeaf::Node),
            url: self.api_base.format_url(format_args!("/users/{id}")),
            _phantom: PhantomData,
        }
    }
    pub fn list_users(&self) -> EdgeReq<'static, 'static, model::User> {
        EdgeReq {
            ty: ArchiveEdge(EdgeBranch::Edge(ArchiveSelfEdge::User)),
            base_url: self.api_base.edge_url_with_param("/users", &[]),
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
}
impl<'s, 'h> PostHandler<'s, 'h> {
    pub fn get(&self) -> NodeReq<'s, 'h, model::Post> {
        self.handler.blog_node_req(
            BlogChildNode::Post(self.id, NodeLeaf::Node),
            format_args!("/posts/{}", self.id),
        )
    }
    pub fn list_comments(&self) -> EdgeReq<'s, 'h, model::Comment> {
        self.handler.blog_child_edge_req(
            BlogChildEdge::Post(self.id, EdgeLeaf::Edge(PostEdge::Comment)),
            format_args!("/comments"),
            &[("post", &self.id.to_string())],
        )
    }
}
