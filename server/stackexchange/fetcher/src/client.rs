use std::{fmt::Display, marker::PhantomData};

use reqwest::{header::HeaderName, Url};
use serde::de::DeserializeOwned;
use uuid::Uuid;

use webar_core::Timestamp;
use webar_data::{bytes::ByteBuf, ser::Never};
use webar_stackexchange_core::{
    api::{
        filter::{FilterSpec, TypeMap},
        model::{
            Answer, ApiObject, Badge, Collective, Comment, Info, Question, Revision, Tag,
            TagSynonym, TagWiki, User, Wrapper,
        },
        request::{self, ApiObjectType, List, ListRequest, Objects, ResponseId},
    },
    fetcher::api_client::{ApiData, ApiResponse, HttpRequest, ListData, ObjectsData},
    id::{AnswerId, BadgeId, CollectiveSlug, CommentId, QuestionId, RevisionId, TagName, UserId},
    KnownSite,
};

use crate::NonEmpty;

struct ManyId<'a, T, I>(&'a NonEmpty<T, I>);
impl<'a, T: Display, I: AsRef<[T]>> Display for ManyId<'a, T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0 .0)?;
        for i in self.0 .1.as_ref() {
            write!(f, ";{i}")?;
        }
        Ok(())
    }
}

const CURRENT_VERSION: webar_stackexchange_core::api::ApiVersion =
    webar_stackexchange_core::api::ApiVersion::V2_3;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("http error: {0}")]
    Http(#[source] reqwest::Error),
    #[error("failed to decode response {0:?}: {1}")]
    Json(Option<ResponseId>, #[source] serde_json::Error),
}

pub struct Client<FN> {
    pub seq: u32,
    pub site: KnownSite,
    pub runtime: tokio::runtime::Handle,
    pub filter: TypeMap<FilterSpec<FN>>,
    pub client: reqwest::Client,
}
pub struct Response<O> {
    response: HttpRequest,
    parsed: Wrapper<O>,
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Order {
    Asc,
    #[default]
    Desc,
}
impl Order {
    fn add_url(self, url: &mut Url) {
        url.query_pairs_mut().append_pair(
            "order",
            match self {
                Self::Asc => "asc",
                Self::Desc => "desc",
            },
        );
    }
}

pub trait ObjectApi {
    const TYPE: ApiObjectType;
    type Output: DeserializeOwned;

    fn into_url(self) -> Url;
}

pub struct ListReq<E: ?Sized, S> {
    base_url: Url,
    pub(crate) request: ListRequest<S>,
    responses: Vec<HttpRequest>,
    _phantom: PhantomData<E>,
}

pub trait ListApi {
    type Output: ApiObject;
    type Str;
    fn into_req(self) -> ListReq<Self, Self::Str>;
}

macro_rules! format_url {
    ($p:expr, $ids:expr) => {
        format!(concat!("https://api.stackexchange.com/2.3", $p), ids = $ids)
    };
}

impl<FN> Client<FN> {
    #[tracing::instrument(skip(self, url), err, fields(url = %url))]
    async fn get<O: DeserializeOwned>(&self, url: Url, filter: &str) -> Result<Response<O>, Error> {
        const X_REQUEST_ID: HeaderName = HeaderName::from_static("x-request-id");
        const X_REQUEST_GUID: HeaderName = HeaderName::from_static("x-request-guid");

        let request_id = uuid::Uuid::new_v4();
        let req = self
            .client
            .get(url)
            .query(&[
                ("site", self.site.as_ref()),
                ("filter", filter),
                ("pagesize", "100"),
            ])
            .header(X_REQUEST_ID, request_id.to_string())
            .build()
            .map_err(Error::Http)?;
        let request = request::Request {
            id: request::RequestId::XRequestId(request_id),
            method: reqwest::Method::GET.into(),
            url: req.url().to_string(),
            timestamp: Timestamp::now(),
            body: (),
        };
        tracing::info!(
            url = tracing::field::display(&request.url),
            request_id = tracing::field::display(request_id),
            "sending request"
        );
        let resp = self
            .client
            .execute(req)
            .await
            .map_err(Error::Http)?
            .error_for_status()
            .map_err(Error::Http)?;
        tracing::debug!(response = tracing::field::debug(&resp), "get response");
        let response_id = match resp.headers().get(X_REQUEST_GUID) {
            Some(v) => match Uuid::try_parse_ascii(v.as_bytes()) {
                Ok(v) => {
                    tracing::info!(x_request_guid = tracing::field::debug(v), "response id");
                    Some(ResponseId::XRequestGuid(v))
                }
                Err(e) => {
                    tracing::warn!(
                        "failed to parse response id header {X_REQUEST_GUID} {v:?}: {e}"
                    );
                    None
                }
            },
            None => {
                tracing::warn!("response id header {X_REQUEST_GUID} not found");
                None
            }
        };
        let status = resp.status().into();
        let headers = resp.headers().to_owned().into();
        let body = resp.bytes().await.map_err(Error::Http)?;
        let timestamp = Timestamp::now();
        Ok(Response {
            parsed: serde_json::from_slice(&body)
                .map_err(|e| Error::Json(response_id.clone(), e))?,
            response: HttpRequest {
                request,
                response: request::Response {
                    id: response_id,
                    status,
                    timestamp,
                    headers,
                    body: ByteBuf(body.to_vec()),
                },
            },
        })
    }
}
impl<FN: AsRef<str>> Client<FN> {
    pub fn request_list<E: ListApi>(
        &self,
        request: &mut ListReq<E, E::Str>,
        page: usize,
    ) -> Result<Wrapper<Vec<E::Output>>, Error> {
        let mut url = request.base_url.clone();
        url.query_pairs_mut().append_pair("page", &page.to_string());
        let ret = self
            .runtime
            .block_on(self.get(url, self.filter[E::Output::TYPE].name.as_ref()))?;
        request.responses.push(ret.response);
        Ok(ret.parsed)
    }
}
impl<FN> Client<FN> {
    pub fn finish_list<E: ListApi>(
        &mut self,
        request: ListReq<E, E::Str>,
        full: bool,
    ) -> ApiResponse<ListData<E::Str>> {
        ApiResponse {
            site: self.site,
            seq: {
                let r = self.seq;
                self.seq += 1;
                r
            },
            api_version: CURRENT_VERSION,
            filter: self.filter[E::Output::TYPE].id,
            data: List {
                request: request.request,
                full,
                responses: request.responses,
            },
        }
    }
}
impl<FN: AsRef<str>> Client<FN> {
    pub fn request_object<E: ObjectApi>(
        &mut self,
        request: E,
    ) -> Result<ApiData<ObjectsData, E::Output>, Error> {
        self.runtime
            .block_on(self.get(request.into_url(), self.filter[E::TYPE].name.as_ref()))
            .map(|r| ApiData {
                response: ApiResponse {
                    site: self.site,
                    seq: {
                        let r = self.seq;
                        self.seq += 1;
                        r
                    },
                    api_version: CURRENT_VERSION,
                    filter: self.filter[E::TYPE].id,
                    data: Objects {
                        ty: E::TYPE,
                        response: r.response,
                    },
                },
                parsed: r.parsed,
            })
    }
}

pub struct GetInfo;
impl ObjectApi for GetInfo {
    const TYPE: ApiObjectType = ApiObjectType::Info;
    type Output = [Info<serde_json::Value>; 1];
    fn into_url(self) -> Url {
        Url::parse("https://api.stackexchange.com/2.3/info").unwrap()
    }
}

pub struct GetObjects<O> {
    url: Url,
    _phantom: PhantomData<fn() -> O>,
}
impl<O> GetObjects<O> {
    fn new(url: &str) -> Self {
        Self {
            url: Url::parse(url).unwrap(),
            _phantom: PhantomData,
        }
    }
}
impl<O: ApiObject> ObjectApi for GetObjects<O> {
    const TYPE: ApiObjectType = O::TYPE;
    type Output = Vec<O>;
    fn into_url(self) -> Url {
        self.url
    }
}

trait Sort: Copy {
    fn to_sort(self) -> &'static str;
}

macro_rules! sort {
    (enum $n:ident {$($s:ident = $v:literal,)+}) => {
        #[derive(Clone, Copy, PartialEq, Eq, Default)]
        pub enum $n {
            #[default]
            $($s),+
        }
        impl Sort for $n {
            fn to_sort(self) -> &'static str {
                match self {
                    $(Self::$s => $v),+
                }
            }
        }
    };
}
sort!(
    enum SortCV {
        Creation = "creation",
        Vote = "vote",
    }
);
sort!(
    enum SortACV {
        Activity = "activity",
        Creation = "creation",
        Vote = "votes",
    }
);
sort!(
    enum SortPAN {
        Popular = "popular",
        Activity = "activity",
        Name = "name",
    }
);
sort!(
    enum SortRCNM {
        Reputation = "reputation",
        Creation = "creation",
        Name = "name",
        Modified = "modified",
    }
);
sort!(
    enum SortRNT {
        Rank = "rank",
        Name = "name",
        Type = "type",
    }
);

sort!(
    enum SortCAA {
        Creation = "creation",
        Applied = "applied",
        Activity = "activity",
    }
);
sort!(
    enum SortRNTA {
        Rank = "rank",
        Name = "name",
        Type = "type",
        Awarded = "awarded",
    }
);

pub struct PagedObjects<S, O, LS> {
    request: ListRequest<LS>,
    url: Url,

    pub order: Order,
    pub sort: S,
    _phantom: PhantomData<fn() -> O>,
}
impl<LS, S: Default, O> PagedObjects<S, O, LS> {
    fn new(request: ListRequest<LS>, url: &str) -> Self {
        Self {
            request,
            url: Url::parse(url).unwrap(),
            order: Order::default(),
            sort: S::default(),
            _phantom: PhantomData,
        }
    }
    pub fn order(&mut self, order: Order) -> &mut Self {
        self.order = order;
        self
    }
    pub fn sort(&mut self, sort: S) -> &mut Self {
        self.sort = sort;
        self
    }
}
impl<LS, S: Sort, O: ApiObject> ListApi for PagedObjects<S, O, LS> {
    type Output = O;
    type Str = LS;
    fn into_req(mut self) -> ListReq<Self, Self::Str> {
        self.url
            .query_pairs_mut()
            .append_pair("sort", self.sort.to_sort());
        self.order.add_url(&mut self.url);
        ListReq {
            request: self.request,
            base_url: self.url,
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

pub type PagedComments<LR> = PagedObjects<SortCV, Comment, LR>;

pub type PagedAnswers<LR> = PagedObjects<SortACV, Answer, LR>;

pub type PagedBadges<LR> = PagedObjects<SortRNT, Badge, LR>;

pub type PagedQuestions<LR> = PagedObjects<SortACV, Question, LR>;

pub type PagedTags<LR> = PagedObjects<SortPAN, Tag, LR>;

pub type PagedTagSynonyms<LR> = PagedObjects<SortCAA, TagSynonym, LR>;

pub type PagedUsers<LR> = PagedObjects<SortRCNM, User, LR>;

pub struct ListRevision<S> {
    request: ListRequest<S>,
    base_url: Url,
    _phantom: PhantomData<S>,
}
impl<S> ListRevision<S> {
    fn new(request: ListRequest<S>, url: &str) -> Self {
        Self {
            request,
            base_url: Url::parse(url).unwrap(),
            _phantom: PhantomData,
        }
    }
}
impl<S> ListApi for ListRevision<S> {
    type Output = Revision;
    type Str = S;
    fn into_req(self) -> ListReq<Self, Self::Str> {
        ListReq {
            request: self.request,
            base_url: self.base_url,
            responses: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

pub struct AnswersHandler<I>(pub NonEmpty<AnswerId, I>);
impl<I: AsRef<[AnswerId]>> AnswersHandler<I> {
    pub fn get(&self) -> GetObjects<Answer> {
        GetObjects::new(&format_url!("/answers/{ids}", ManyId(&self.0)))
    }
    pub fn questions(&self) -> GetObjects<Question> {
        GetObjects::new(&format_url!("/answers/{ids}/questions", ManyId(&self.0)))
    }
}

pub struct AnswerHandler(pub AnswerId);
impl AnswerHandler {
    pub fn comments(&self) -> PagedComments<Never> {
        PagedComments::new(
            ListRequest::Answer {
                id: self.0.clone(),
                request: request::AnswerListReq::Comment,
            },
            &format_url!("/answers/{ids}/comments", self.0),
        )
    }
    pub fn revisions(&self) -> ListRevision<Never> {
        ListRevision::new(
            ListRequest::Answer {
                id: self.0,
                request: request::AnswerListReq::Revision,
            },
            &format_url!("/posts/{ids}/revisions", self.0),
        )
    }
}

pub struct BadgesHandler<I>(pub NonEmpty<BadgeId, I>);
impl<I: AsRef<[BadgeId]>> BadgesHandler<I> {
    pub fn get(&self) -> GetObjects<Badge> {
        GetObjects::new(&format_url!("/badges/{ids}", ManyId(&self.0)))
    }
}

pub struct CollectivesHandler<S, I>(pub NonEmpty<CollectiveSlug<S>, I>);
impl<S: AsRef<str>, I: AsRef<[CollectiveSlug<S>]>> CollectivesHandler<S, I> {
    pub fn get(&self) -> GetObjects<Collective> {
        GetObjects::new(&format_url!("/collectives/{ids}", ManyId(&self.0)))
    }
}

pub struct CollectiveHandler<S>(pub CollectiveSlug<S>);
impl<S: AsRef<str> + Clone> CollectiveHandler<S> {
    pub fn answers(&self) -> PagedAnswers<S> {
        PagedAnswers::new(
            ListRequest::Collective {
                id: self.0.clone(),
                request: request::CollectiveListReq::Answer,
            },
            &format_url!("/collectives/{ids}/answers", self.0),
        )
    }
    pub fn questions(&self) -> PagedQuestions<S> {
        PagedQuestions::new(
            ListRequest::Collective {
                id: self.0.clone(),
                request: request::CollectiveListReq::Question,
            },
            &format_url!("/collectives/{ids}/questions", self.0),
        )
    }
    pub fn tags(&self) -> PagedTags<S> {
        PagedTags::new(
            ListRequest::Collective {
                id: self.0.clone(),
                request: request::CollectiveListReq::Tag,
            },
            &format_url!("/collectives/{ids}/tags", self.0),
        )
    }
    pub fn users(&self) -> PagedUsers<S> {
        PagedObjects::new(
            ListRequest::Collective {
                id: self.0.clone(),
                request: request::CollectiveListReq::User,
            },
            &format_url!("/collectives/{ids}/users", self.0),
        )
    }
}

pub struct CommentsHandler<I>(pub NonEmpty<CommentId, I>);
impl<I: AsRef<[CommentId]>> CommentsHandler<I> {
    pub fn get(&self) -> GetObjects<Comment> {
        GetObjects::new(&format_url!("/comments/{ids}", ManyId(&self.0)))
    }
}

pub struct QuestionsHandler<I>(pub NonEmpty<QuestionId, I>);
impl<I: AsRef<[QuestionId]>> QuestionsHandler<I> {
    pub fn get(&self) -> GetObjects<Question> {
        GetObjects::new(&format_url!("/questions/{ids}", ManyId(&self.0)))
    }
}
pub struct QuestionHandler(pub QuestionId);
impl QuestionHandler {
    pub fn comments(&self) -> PagedComments<Never> {
        PagedComments::new(
            ListRequest::Question {
                id: self.0,
                request: request::QuestionListReq::Comment,
            },
            &format_url!("/questions/{ids}/comments", self.0),
        )
    }
    pub fn answers(&self) -> PagedAnswers<Never> {
        PagedAnswers::new(
            ListRequest::Question {
                id: self.0,
                request: request::QuestionListReq::Answer,
            },
            &format_url!("/questions/{ids}/answers", self.0),
        )
    }
    pub fn revisions(&self) -> ListRevision<Never> {
        ListRevision::new(
            ListRequest::Question {
                id: self.0,
                request: request::QuestionListReq::Revision,
            },
            &format_url!("/posts/{ids}/revisions", self.0),
        )
    }
}

pub struct RevisionHandler(pub RevisionId);
impl RevisionHandler {
    pub fn get(&self) -> ListRevision<Never> {
        ListRevision::new(
            ListRequest::ListRevision(self.0),
            &format_url!("/revisions/{ids}", self.0),
        )
    }
}

pub struct TagsHandler<S, I>(pub NonEmpty<TagName<S>, I>);
impl<S: AsRef<str>, I: AsRef<[TagName<S>]>> TagsHandler<S, I> {
    pub fn get(&self) -> GetObjects<Tag> {
        GetObjects::new(&format_url!("/tags/{ids}/info", ManyId(&self.0)))
    }
    pub fn wikis(&self) -> GetObjects<TagWiki> {
        GetObjects::new(&format_url!("/tags/{ids}/wikis", ManyId(&self.0)))
    }
}
pub struct TagHandler<S>(pub TagName<S>);
impl<S: AsRef<str> + Clone> TagHandler<S> {
    pub fn synonyms(&self) -> PagedTagSynonyms<S> {
        PagedTagSynonyms::new(
            ListRequest::Tag {
                id: self.0.clone(),
                request: request::TagListReq::TagSynonym,
            },
            &format_url!("/tags/{ids}/synonyms", self.0),
        )
    }
}

pub struct UsersHandler<I>(pub NonEmpty<UserId, I>);
impl<I: AsRef<[UserId]>> UsersHandler<I> {
    pub fn get(&self) -> GetObjects<User> {
        GetObjects::new(&format_url!("/users/{ids}", ManyId(&self.0)))
    }
}
pub struct UserHandler(pub UserId);
impl UserHandler {
    pub fn answers(&self) -> PagedAnswers<Never> {
        PagedAnswers::new(
            ListRequest::User {
                id: self.0,
                request: request::UserListReq::Answer,
            },
            &format_url!("/users/{ids}/answers", self.0),
        )
    }
    pub fn badges(&self) -> PagedObjects<SortRNTA, Badge, Never> {
        PagedObjects::new(
            ListRequest::User {
                id: self.0,
                request: request::UserListReq::Badge,
            },
            &format_url!("/users/{ids}/badges", self.0),
        )
    }
    pub fn questions(&self) -> PagedQuestions<Never> {
        PagedQuestions::new(
            ListRequest::User {
                id: self.0,
                request: request::UserListReq::Question,
            },
            &format_url!("/users/{ids}/questions", self.0),
        )
    }
    pub fn comments(&self) -> PagedComments<Never> {
        PagedComments::new(
            ListRequest::User {
                id: self.0,
                request: request::UserListReq::Comment,
            },
            &format_url!("/users/{ids}/comments", self.0),
        )
    }
}
