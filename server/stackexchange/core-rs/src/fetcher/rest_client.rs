use serde::Deserialize;

use webar_core::http;
use webar_data::{bytes::ByteBuf, ser::Serialize};

use crate::rest_api::{source, ApiInfo};

pub type RequestMeta = http::RequestMeta<source::RequestId, ()>;
pub type ResponseMeta = http::ResponseMeta<Option<source::ResponseId>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpMeta {
    pub request: RequestMeta,
    pub response: ResponseMeta,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpRequest {
    pub request: source::Request<String>,
    pub response: source::Response<http::HeaderMap<http::HeaderValue>, ByteBuf>,
}
impl HttpRequest {
    fn to_meta(&self) -> HttpMeta {
        HttpMeta {
            request: http::RequestMeta {
                id: self.request.id.clone(),
                timestamp: self.request.timestamp,
                body: (),
            },
            response: http::ResponseMeta {
                id: self.response.id.clone(),
                timestamp: self.response.timestamp,
            },
        }
    }
}

pub type ResponseData<S> = source::ResponseData<S, HttpRequest>;
impl<S: Copy> ResponseData<S> {
    fn to_meta(&self) -> source::ResponseData<S, HttpMeta> {
        match self {
            Self::Node { ty, response } => source::ResponseData::Node {
                ty: *ty,
                response: response.to_meta(),
            },
            Self::Edge {
                ty,
                full,
                responses,
            } => source::ResponseData::Edge {
                ty: *ty,
                full: *full,
                responses: responses.iter().map(HttpRequest::to_meta).collect(),
            },
            Self::Revision { ty, responses } => source::ResponseData::Revision {
                ty: *ty,
                responses: responses.iter().map(HttpRequest::to_meta).collect(),
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiResponse<S, R> {
    pub api: ApiInfo,
    pub seq: u32,
    pub data: source::ResponseData<S, R>,
}
impl<S: Copy> ApiResponse<S, HttpRequest> {
    pub fn to_meta(&self) -> ApiResponse<S, HttpMeta> {
        ApiResponse {
            api: self.api,
            seq: self.seq,
            data: self.data.to_meta(),
        }
    }
}
