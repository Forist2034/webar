use std::{thread::sleep, time::Duration};

use reqwest::{self, header::HeaderName, Url};
use serde::de::DeserializeOwned;

use webar_core::Timestamp;
use webar_data::bytes::ByteBuf;
use webar_wordpress_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest, ResponseData},
    rest_api::{source, ApiInfo},
};

use crate::handler::{EdgeReq, NodeReq, StrAddr, CURRENT_API};

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

pub struct Config {
    pub retry_count: u16,
    pub retry_wait: Duration,
    pub wait: Duration,
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
    pub fn request_edge<O: DeserializeOwned>(
        &self,
        req: &mut EdgeReq<O>,
        offset: usize,
    ) -> Result<(Vec<O>, Paging), Error> {
        let _span =
            tracing::info_span!("request_edge", ty = tracing::field::debug(&req.ty), offset)
                .entered();
        let ret = self.get_page_retry(&req.base_url, offset)?;
        req.responses.push(ret.response);
        Ok((ret.parsed, ret.paging))
    }
}
