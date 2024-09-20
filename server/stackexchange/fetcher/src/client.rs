use std::num::NonZeroUsize;

use reqwest::{header::HeaderName, Url};
use serde::{de::DeserializeOwned, Deserialize};
use uuid::Uuid;

use webar_core::Timestamp;
use webar_data::bytes::ByteBuf;
use webar_stackexchange_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest, ResponseData},
    rest_api::{model, source, ApiInfo},
};

use crate::handler::{EdgeReq, NodeReq, RevisionReq, API_VERSION};

const X_REQUEST_ID: HeaderName = HeaderName::from_static("x-request-id");
const X_REQUEST_GUID: HeaderName = HeaderName::from_static("x-request-guid");

pub struct NodeData<'s, O> {
    pub response: ApiResponse<&'s str, HttpRequest>,
    pub data: model::Wrapper<Option<O>>,
}
pub struct RevisionData<'s> {
    pub response: ApiResponse<&'s str, HttpRequest>,
    pub data: Vec<model::Wrapper<Vec<model::Revision>>>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("http error: {0}")]
    Http(
        #[from]
        #[source]
        reqwest::Error,
    ),
    #[error("failed to decode response: {0}")]
    Json(#[source] serde_json::Error),
}

#[derive(Deserialize)]
#[serde(untagged)]
enum NodeBody<O> {
    None([O; 0]),
    One([O; 1]),
}
struct Response<O> {
    response: HttpRequest,
    parsed: model::Wrapper<O>,
}
pub struct Client {
    pub runtime: tokio::runtime::Handle,
    pub client: reqwest::Client,
}
impl Client {
    #[tracing::instrument(skip(self, url), err, fields(url = %url))]
    async fn get<O: DeserializeOwned>(
        &self,
        url: &Url,
        page: Option<NonZeroUsize>,
    ) -> Result<Response<O>, Error> {
        let request_id = Uuid::new_v4();
        let req = match page {
            Some(p) => self.client.get(url.clone()).query(&[("page", p)]),
            None => self.client.get(url.clone()),
        }
        .header(X_REQUEST_ID, request_id.to_string())
        .build()
        .map_err(Error::Http)?;
        let request = source::Request {
            id: source::RequestId::XRequestId(request_id),
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
                    Some(source::ResponseId::XRequestGuid(v))
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
        let parsed: model::Wrapper<O> = serde_json::from_slice(&body).map_err(Error::Json)?;
        if let Some(b) = parsed.backoff {
            tracing::info!(backoff = b, "received backoff");
            tokio::time::sleep(std::time::Duration::from_secs(b)).await;
        }
        Ok(Response {
            parsed,
            response: HttpRequest {
                request,
                response: source::Response {
                    id: response_id,
                    status,
                    timestamp,
                    headers,
                    body: ByteBuf(body.to_vec()),
                },
            },
        })
    }

    pub fn request_node<'s, O: DeserializeOwned>(
        &self,
        seq: u32,
        req: NodeReq<'s, O>,
    ) -> Result<NodeData<'s, O>, Error> {
        let _span =
            tracing::info_span!("request_node", ty = tracing::field::debug(&req.ty)).entered();
        self.runtime
            .block_on(self.get(&req.url, None))
            .map(|r| NodeData {
                response: ApiResponse {
                    api: ApiInfo {
                        version: API_VERSION,
                        filter: req.filter,
                    },
                    seq,
                    data: ResponseData::Node {
                        ty: req.ty,
                        response: r.response,
                    },
                },
                data: model::Wrapper {
                    backoff: r.parsed.backoff,
                    error_id: r.parsed.error_id,
                    error_message: r.parsed.error_message,
                    error_name: r.parsed.error_name,
                    has_more: r.parsed.has_more,
                    quota_remaining: r.parsed.quota_remaining,
                    items: match r.parsed.items {
                        NodeBody::One([v]) => Some(v),
                        NodeBody::None([]) => None,
                    },
                },
            })
    }
    pub fn request_edge<O: DeserializeOwned>(
        &self,
        req: &mut EdgeReq<O>,
        page: NonZeroUsize,
    ) -> Result<model::Wrapper<Vec<O>>, Error> {
        let _span = tracing::info_span!("request_edge", ty = tracing::field::debug(&req.ty), page)
            .entered();
        self.runtime
            .block_on(self.get(&req.base_url, Some(page)))
            .map(|r| {
                req.responses.push(r.response);
                r.parsed
            })
    }
    pub fn request_revision<'s>(
        &self,
        seq: u32,
        req: RevisionReq<'s>,
    ) -> Result<RevisionData<'s>, Error> {
        let _span =
            tracing::info_span!("request_revision", ty = tracing::field::debug(&req.ty)).entered();
        self.runtime
            .block_on(async {
                let mut data = Vec::new();
                let mut resp = Vec::new();
                let mut page = NonZeroUsize::new(1).unwrap();
                loop {
                    let r = self.get(&req.base_url, Some(page)).await?;
                    resp.push(r.response);
                    let has_more = r.parsed.has_more;
                    data.push(r.parsed);
                    if has_more {
                        page = page.checked_add(1).unwrap();
                    } else {
                        break Ok((resp, data));
                    }
                }
            })
            .map(|(resp, data)| RevisionData {
                response: ApiResponse {
                    api: ApiInfo {
                        version: API_VERSION,
                        filter: req.filter,
                    },
                    seq,
                    data: ResponseData::Revision {
                        ty: req.ty,
                        responses: resp,
                    },
                },
                data,
            })
    }
}
