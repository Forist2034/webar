use serde::Deserialize;

use webar_core::http;
use webar_data::{bytes::ByteBuf, ser::Serialize};

use crate::rest_api::{source, ApiInfo};

pub type RequestMeta = http::RequestMeta<source::RequestId, (), ()>;

pub type ResponseMeta = http::ResponseMeta<()>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpMeta {
    pub request: RequestMeta,
    pub response: ResponseMeta,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HttpRequest {
    pub request: source::Request<String>,
    pub response: source::Response<ByteBuf>,
}
impl HttpRequest {
    fn to_meta(&self) -> HttpMeta {
        HttpMeta {
            request: RequestMeta {
                id: self.request.id.clone(),
                timestamp: self.request.timestamp,
                headers: (),
                body: (),
            },
            response: ResponseMeta {
                id: (),
                timestamp: self.response.timestamp,
            },
        }
    }
}

pub type ResponseData<Addr> = source::ResponseData<Addr, HttpRequest>;
impl<Addr: Copy> ResponseData<Addr> {
    pub fn to_meta(&self) -> source::ResponseData<Addr, HttpMeta> {
        match self {
            Self::Node { ty, response } => source::ResponseData::Node {
                ty: *ty,
                response: response.to_meta(),
            },
            Self::Edge {
                ty,
                full,
                responses: response,
            } => source::ResponseData::Edge {
                ty: *ty,
                full: *full,
                responses: response.iter().map(HttpRequest::to_meta).collect(),
            },
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ApiResponse<A, R> {
    pub api: ApiInfo,
    pub seq: u32,
    pub data: source::ResponseData<A, R>,
}
impl<A: Copy> ApiResponse<A, HttpRequest> {
    pub fn to_meta(&self) -> ApiResponse<A, HttpMeta> {
        ApiResponse {
            api: self.api.clone(),
            seq: self.seq,
            data: self.data.to_meta(),
        }
    }
}
