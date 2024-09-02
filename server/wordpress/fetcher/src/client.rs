use std::{fmt, marker::PhantomData, thread::sleep, time::Duration};

use reqwest::{self, header::HeaderName, Url};
use serde::de::DeserializeOwned;

use webar_core::Timestamp;
use webar_data::bytes::ByteBuf;
use webar_wordpress_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest, ResponseData},
    id::{
        CategoryId, CommentId, MediaId, PageId, PageRevisionId, PostId, PostRevisionId, TagId,
        UserId,
    },
    rest_api::{
        model, source,
        source::{
            ArchiveBlogCollection, ArchiveBlogEdge, ArchiveBlogNode, ArchiveCollection,
            ArchiveEdge, ArchiveNode, ArchivePageEdge, ArchivePostEdge,
        },
        ApiInfo, ApiVersion,
    },
    Address,
};

const CURRENT_API: ApiVersion = ApiVersion::V2;
const PER_PAGE_STR: &str = "100";

pub struct Config {
    pub retry_count: u16,
    pub retry_wait: Duration,
    pub wait: Duration,
}

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
    fn collection_url(&self, route: impl fmt::Display) -> Url {
        self.edge_url_with_param(route, &[])
    }
}

pub type StrAddr<'s> = Address<&'s str, &'s str>;

#[derive(Debug, Clone, Copy)]
pub struct Paging {
    pub total: usize,
    pub total_page: usize,
}
pub struct ApiData<'s, 'a, I, P> {
    pub response: ApiResponse<&'a StrAddr<'s>, HttpRequest>,
    pub parsed: I,
    pub paging: P,
}

#[derive(Debug)]
pub(crate) enum PageType<'s, 'a> {
    Edge(ArchiveEdge<&'a StrAddr<'s>>),
    Collection(ArchiveCollection<&'a StrAddr<'s>>),
}
pub struct NodeReq<'s, 'a, O> {
    pub(crate) ty: ArchiveNode<&'a StrAddr<'s>>,
    url: Url,
    _phantom: PhantomData<fn() -> O>,
}
pub struct PageReq<'s, 'a, O> {
    pub(crate) ty: PageType<'s, 'a>,
    base_url: Url,
    responses: Vec<HttpRequest>,
    _phantom: PhantomData<fn() -> O>,
}
impl<'a, 's, O> PageReq<'s, 'a, O> {
    pub fn finish(self, seq: u32, full: bool) -> ApiResponse<&'a StrAddr<'s>, HttpRequest> {
        ApiResponse {
            api: ApiInfo {
                version: CURRENT_API,
            },
            seq,
            data: match self.ty {
                PageType::Edge(ty) => source::ResponseData::Edge {
                    ty,
                    full,
                    responses: self.responses,
                },
                PageType::Collection(ty) => ResponseData::Collection {
                    ty,
                    full,
                    responses: self.responses,
                },
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
pub struct PageHandler<'s, 'h> {
    handler: &'h Handler<'s>,
    id: PageId,
}
impl<'s> Handler<'s> {
    fn blog_node_req<'h, O>(
        &'h self,
        ty: ArchiveBlogNode,
        route: fmt::Arguments,
    ) -> NodeReq<'s, 'h, O> {
        NodeReq {
            ty: ArchiveNode::Blog(&self.blog, ty),
            url: self.api_base.format_url(route),
            _phantom: PhantomData,
        }
    }
    fn blog_edge_req<'h, O>(
        &'h self,
        ty: ArchiveBlogEdge,
        route: fmt::Arguments,
        params: &[(&str, &str)],
    ) -> PageReq<'s, 'h, O> {
        PageReq {
            ty: PageType::Edge(ArchiveEdge::Blog(&self.blog, ty)),
            base_url: self.api_base.edge_url_with_param(route, params),
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
    fn blog_collection_req<'h, O>(
        &'h self,
        ty: ArchiveBlogCollection,
        route: &str,
    ) -> PageReq<'s, 'h, O> {
        PageReq {
            ty: PageType::Collection(ArchiveCollection::Blog(&self.blog, ty)),
            base_url: self.api_base.collection_url(route),
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn get_category<'h>(&'h self, id: CategoryId) -> NodeReq<'s, 'h, model::Category> {
        self.blog_node_req(
            ArchiveBlogNode::Category(id),
            format_args!("/categories/{id}"),
        )
    }
    pub fn list_categories<'h>(&'h self) -> PageReq<'s, 'h, model::Category> {
        self.blog_collection_req(ArchiveBlogCollection::Category, "/categories")
    }

    pub fn get_comment<'h>(&'h self, id: CommentId) -> NodeReq<'s, 'h, model::Comment> {
        self.blog_node_req(ArchiveBlogNode::Comment(id), format_args!("/comments/{id}"))
    }
    pub fn list_comments<'h>(&'h self) -> PageReq<'s, 'h, model::Comment> {
        self.blog_collection_req(ArchiveBlogCollection::Comment, "/comments")
    }

    pub fn get_media<'h>(&'h self, id: MediaId) -> NodeReq<'s, 'h, model::Media> {
        self.blog_node_req(ArchiveBlogNode::Media(id), format_args!("/media/{id}"))
    }
    pub fn list_media<'h>(&'h self) -> PageReq<'s, 'h, model::Media> {
        self.blog_collection_req(ArchiveBlogCollection::Media, "/media")
    }

    pub fn page<'h>(&'h self, id: PageId) -> PageHandler<'s, 'h> {
        PageHandler { handler: self, id }
    }
    pub fn list_pages<'h>(&'h self) -> PageReq<'s, 'h, model::Page> {
        self.blog_collection_req(ArchiveBlogCollection::Page, "/pages")
    }

    pub fn post<'h>(&'h self, id: PostId) -> PostHandler<'s, 'h> {
        PostHandler { handler: self, id }
    }
    pub fn list_posts<'h>(&'h self) -> PageReq<'s, 'h, model::Post> {
        self.blog_collection_req(ArchiveBlogCollection::Post, "/posts")
    }

    pub fn get_tag<'h>(&'h self, id: TagId) -> NodeReq<'s, 'h, model::Tag> {
        self.blog_node_req(ArchiveBlogNode::Tag(id), format_args!("/tags/{id}"))
    }
    pub fn list_tags<'h>(&'h self) -> PageReq<'s, 'h, model::Tag> {
        self.blog_collection_req(ArchiveBlogCollection::Tag, "/tags")
    }

    pub fn get_user<'h>(&'h self, id: UserId) -> NodeReq<'s, 'h, model::User> {
        NodeReq {
            ty: ArchiveNode::User(id),
            url: self.api_base.format_url(format_args!("/users/{id}")),
            _phantom: PhantomData,
        }
    }
    pub fn list_users<'h>(&self) -> PageReq<'s, 'h, model::User> {
        PageReq {
            ty: PageType::Collection(ArchiveCollection::User),
            base_url: self.api_base.collection_url("/users"),
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
}
impl<'s, 'h> PageHandler<'s, 'h> {
    pub fn get(&self) -> NodeReq<'s, 'h, model::Page> {
        self.handler.blog_node_req(
            ArchiveBlogNode::Page(self.id),
            format_args!("/pages/{}", self.id),
        )
    }
    pub fn get_revision(&self, rev_id: PageRevisionId) -> NodeReq<'s, 'h, model::PageRevision> {
        self.handler.blog_node_req(
            ArchiveBlogNode::PageRevision(self.id, rev_id),
            format_args!("/pages/{}/revisions/{rev_id}", self.id),
        )
    }
    pub fn list_revisions(&self) -> PageReq<'s, 'h, model::PageRevision> {
        self.handler.blog_edge_req(
            ArchiveBlogEdge::Page(self.id, ArchivePageEdge::Revision),
            format_args!("/posts/{}/revisions", self.id),
            &[],
        )
    }
}
impl<'s, 'h> PostHandler<'s, 'h> {
    pub fn get(&self) -> NodeReq<'s, 'h, model::Post> {
        self.handler.blog_node_req(
            ArchiveBlogNode::Post(self.id),
            format_args!("/posts/{}", self.id),
        )
    }
    pub fn get_revision(&self, rev_id: PostRevisionId) -> NodeReq<'s, 'h, model::PostRevision> {
        self.handler.blog_node_req(
            ArchiveBlogNode::PostRevision(self.id, rev_id),
            format_args!("/posts/{}/revisions/{rev_id}", self.id),
        )
    }
    pub fn list_revisions(&self) -> PageReq<'s, 'h, model::PostRevision> {
        self.handler.blog_edge_req(
            ArchiveBlogEdge::Post(self.id, ArchivePostEdge::Revision),
            format_args!("/posts/{}/revisions", self.id),
            &[],
        )
    }
    pub fn list_comments(&self) -> PageReq<'s, 'h, model::Comment> {
        self.handler.blog_edge_req(
            ArchiveBlogEdge::Post(self.id, ArchivePostEdge::Comment),
            format_args!("/comments"),
            &[("post", &self.id.to_string())],
        )
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PagingHeaderError {
    #[error("missing header")]
    Missing,
    #[error("invalid utf8")]
    InvalidUtf8(#[source] reqwest::header::ToStrError),
    #[error("invalid value {value:?}")]
    InvalidValue {
        value: String,
        #[source]
        source: std::num::ParseIntError,
    },
}
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("http error")]
    Http(
        #[source]
        #[from]
        reqwest::Error,
    ),
    #[error("json error")]
    Json(#[source] serde_json::Error),
    #[error("failed to parse paging header {header}")]
    PagingHeader {
        header: &'static HeaderName,
        #[source]
        source: PagingHeaderError,
    },
}
pub struct Client {
    pub config: Config,
    pub runtime: tokio::runtime::Handle,
    pub client: reqwest::Client,
}
const X_REQUEST_ID: HeaderName = HeaderName::from_static("x-request-id");
static X_WP_TOTAL: HeaderName = HeaderName::from_static("x-wp-total");
static X_WP_TOTAL_PAGES: HeaderName = HeaderName::from_static("x-wp-totalpages");

fn parse_paging_header(
    headers: &reqwest::header::HeaderMap,
    name: &'static HeaderName,
) -> Result<usize, Error> {
    fn inner(
        headers: &reqwest::header::HeaderMap,
        name: &'static HeaderName,
    ) -> Result<usize, PagingHeaderError> {
        let value = headers
            .get(name)
            .ok_or(PagingHeaderError::Missing)?
            .to_str()
            .map_err(PagingHeaderError::InvalidUtf8)?;
        value.parse().map_err(|e| PagingHeaderError::InvalidValue {
            value: value.to_string(),
            source: e,
        })
    }
    inner(headers, name).map_err(|e| Error::PagingHeader {
        header: name,
        source: e,
    })
}

struct Response<O, P> {
    parsed: O,
    response: HttpRequest,
    paging: P,
}

impl Client {
    async fn get_node<O: DeserializeOwned>(&self, url: Url) -> Result<Response<O, ()>, Error> {
        let request_id = uuid::Uuid::new_v4();
        let req = self
            .client
            .get(url)
            .header(X_REQUEST_ID, request_id.to_string())
            .build()?;
        let request = source::Request {
            id: source::RequestId::XRequestId(request_id),
            method: req.method().clone().into(),
            url: req.url().as_str().to_string(),
            timestamp: Timestamp::now(),
            body: (),
        };
        tracing::info!(
            request_id = tracing::field::display(&request_id),
            "sending request"
        );
        let resp = self.client.execute(req).await?.error_for_status()?;
        tracing::debug!(response = tracing::field::debug(&resp), "received response");
        let status = resp.status().into();
        let headers = resp.headers().clone().into();
        let body = resp.bytes().await?;
        Ok(Response {
            parsed: serde_json::from_slice(&body).map_err(Error::Json)?,
            response: HttpRequest {
                request,
                response: source::Response {
                    id: (),
                    timestamp: Timestamp::now(),
                    status,
                    headers,
                    body: ByteBuf(body.into()),
                },
            },
            paging: (),
        })
    }
    fn get_node_retry<O: DeserializeOwned>(&self, url: Url) -> Result<Response<O, ()>, Error> {
        let mut cnt = 0;
        loop {
            let _span =
                tracing::info_span!("get_node", url = tracing::field::display(&url), "try" = cnt)
                    .entered();
            let ret = self.runtime.block_on(self.get_node(url.clone()));
            match &ret {
                Ok(_) => (),
                Err(err @ Error::Http(e))
                    if e.is_status()
                        && e.status() == Some(reqwest::StatusCode::TOO_MANY_REQUESTS) =>
                {
                    tracing::warn!(
                        error = err as &dyn std::error::Error,
                        "received http 429, waiting"
                    );
                    sleep(self.config.retry_wait);
                    cnt += 1;
                    if cnt < self.config.retry_count {
                        continue;
                    }
                }
                Err(e) => {
                    tracing::error!(error = e as &dyn std::error::Error);
                }
            }
            sleep(self.config.wait);
            break ret;
        }
    }

    async fn get_page<O: DeserializeOwned>(
        &self,
        url: Url,
        offset: usize,
    ) -> Result<Response<Vec<O>, Paging>, Error> {
        let request_id = uuid::Uuid::new_v4();
        let req = self
            .client
            .get(url)
            .query(&[("offset", offset)])
            .header(X_REQUEST_ID, request_id.to_string())
            .build()?;
        let request = source::Request {
            id: source::RequestId::XRequestId(request_id),
            timestamp: Timestamp::now(),
            method: req.method().clone().into(),
            url: req.url().as_str().to_string(),
            body: (),
        };
        tracing::info!(
            request_id = tracing::field::display(&request_id),
            "sending request"
        );
        let resp = self.client.execute(req).await?.error_for_status()?;
        tracing::debug!(response = tracing::field::debug(&resp), "received response");
        let status = resp.status().into();
        let headers = resp.headers().clone().into();
        let total = parse_paging_header(resp.headers(), &X_WP_TOTAL)?;
        let total_page = parse_paging_header(resp.headers(), &X_WP_TOTAL_PAGES)?;
        let body = resp.bytes().await?;
        Ok(Response {
            parsed: serde_json::from_slice(&body).map_err(Error::Json)?,
            response: HttpRequest {
                request,
                response: source::Response {
                    id: (),
                    timestamp: Timestamp::now(),
                    status,
                    headers,
                    body: ByteBuf(body.into()),
                },
            },
            paging: Paging { total_page, total },
        })
    }
    fn get_page_retry<O: DeserializeOwned>(
        &self,
        url: &Url,
        offset: usize,
    ) -> Result<Response<Vec<O>, Paging>, Error> {
        let mut cnt = 0;
        loop {
            let _span = tracing::info_span!(
                "get_page",
                url = tracing::field::display(url),
                offset,
                "try" = cnt
            )
            .entered();
            let ret = self.runtime.block_on(self.get_page(url.clone(), offset));
            match &ret {
                Ok(_) => (),
                Err(err @ Error::Http(e))
                    if e.is_status()
                        && e.status() == Some(reqwest::StatusCode::TOO_MANY_REQUESTS) =>
                {
                    tracing::warn!(
                        error = err as &dyn std::error::Error,
                        "received 429, waiting"
                    );
                    cnt += 1;
                    if cnt < self.config.retry_count {
                        std::thread::sleep(self.config.retry_wait);
                        continue;
                    }
                }
                Err(e) => {
                    tracing::error!(error = e as &dyn std::error::Error);
                }
            }
            sleep(self.config.wait);
            break ret;
        }
    }

    pub fn request_node<'s, 'h, O: DeserializeOwned>(
        &self,
        seq: u32,
        req: NodeReq<'s, 'h, O>,
    ) -> Result<ApiData<'s, 'h, O, ()>, Error> {
        let _span = tracing::info_span!("request_node", seq, node = tracing::field::debug(&req.ty))
            .entered();
        self.get_node_retry(req.url).map(|r| ApiData {
            response: ApiResponse {
                api: ApiInfo {
                    version: CURRENT_API,
                },
                seq,
                data: ResponseData::Node {
                    ty: req.ty,
                    response: r.response,
                },
            },
            parsed: r.parsed,
            paging: (),
        })
    }
    pub fn request_page<O: DeserializeOwned>(
        &self,
        req: &mut PageReq<O>,
        offset: usize,
    ) -> Result<(Vec<O>, Paging), Error> {
        let _span =
            tracing::info_span!("request_page", ty = tracing::field::debug(&req.ty), offset)
                .entered();
        let ret = self.get_page_retry(&req.base_url, offset)?;
        req.responses.push(ret.response);
        Ok((ret.parsed, ret.paging))
    }
}
