use serde::Deserialize;
use webar_core::{
    http::{HeaderMap, HeaderValue, Method},
    Timestamp,
};
use webar_data::{bytes::ByteBuf, ser::Serialize};

use crate::{
    filter::FilterId, ApiVersion, KnownSite, List, ListRequest, Objects, RequestId, Response,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct HttpRequest {
    pub method: Method,
    pub url: String,
    pub request_id: RequestId,
    pub response: Response<HeaderMap<HeaderValue>, ByteBuf>,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct HttpMeta {
    pub timestamp: Timestamp,
    pub request_id: RequestId,
}
impl HttpRequest {
    pub fn to_meta(&self) -> HttpMeta {
        HttpMeta {
            timestamp: self.response.timestamp,
            request_id: self.request_id.clone(),
        }
    }
}

pub type ObjectsData = Objects<HttpRequest>;

impl ObjectsData {
    pub fn to_meta(&self) -> Objects<HttpMeta> {
        Objects {
            ty: self.ty,
            response: self.response.to_meta(),
        }
    }
}

pub type ListData<S> = List<ListRequest<S>, HttpRequest>;

impl<S> ListData<S> {
    pub fn to_meta(&self) -> List<&ListRequest<S>, HttpMeta> {
        List {
            request: &self.request,
            full: self.full,
            responses: self.responses.iter().map(HttpRequest::to_meta).collect(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct ApiResponse<R> {
    pub site: KnownSite,
    pub seq: u32,
    pub api_version: ApiVersion,
    pub filter: FilterId,
    pub data: R,
}

pub struct ApiData<R, I> {
    pub response: ApiResponse<R>,
    pub parsed: crate::api::Wrapper<I>,
}
