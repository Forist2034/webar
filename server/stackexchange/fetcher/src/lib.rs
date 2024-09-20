use std::{convert::Infallible, fmt::Debug, io, iter::FusedIterator, num::NonZeroUsize};

use serde::de::DeserializeOwned;
use webar_stackexchange_core::{
    fetcher::rest_client::{ApiResponse, HttpRequest},
    rest_api::model,
};

pub mod handler;
pub use handler::Handler;

pub mod client;
pub use client::Client;

pub mod sink;

mod hs_filter;
pub use hs_filter::HS_FILTER_INFO;

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("failed to fetch item")]
    Client(#[source] client::Error),
    #[error("failed to write item")]
    Sink(#[source] sink::Error),
    #[error("failed to run inner")]
    Inner(#[source] E),
}

pub struct Fetcher<MW: io::Write, RW: io::Write> {
    pub seq: u32,
    pub client: Client,
    pub sink: sink::TarSink<MW, RW>,
}

pub struct EdgeIter<'a, 's, O> {
    request: &'a mut handler::EdgeReq<'s, O>,
    client: &'a mut client::Client,
    has_more: &'a mut bool,
    page: NonZeroUsize,
}
impl<'a, 's, O: DeserializeOwned> Iterator for EdgeIter<'a, 's, O> {
    type Item = Result<model::Wrapper<Vec<O>>, client::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if *self.has_more {
            let _span = tracing::info_span!("fetch_page", page = self.page).entered();
            let ret = self.client.request_edge(&mut self.request, self.page);
            if let Ok(ref o) = ret {
                *self.has_more = o.has_more;
                self.page = self.page.checked_add(1).unwrap();
            }
            Some(ret)
        } else {
            None
        }
    }
}
impl<'a, 's, O: DeserializeOwned> FusedIterator for EdgeIter<'a, 's, O> {}

impl<MW, RW> Fetcher<MW, RW>
where
    MW: io::Write,
    RW: io::Write,
{
    pub fn fetch_node<'s, O: DeserializeOwned>(
        &mut self,
        req: handler::NodeReq<'s, O>,
    ) -> Result<client::NodeData<'s, O>, Error<Infallible>> {
        let _span =
            tracing::info_span!("fetch_node", ty = tracing::field::debug(&req.ty)).entered();
        let ret = self
            .client
            .request_node(self.seq, req)
            .map_err(Error::Client)?;
        self.seq += 1;
        self.sink.add_response(&ret.response).map_err(Error::Sink)?;
        Ok(ret)
    }

    pub fn with_edge_iter<'s, T, E, O: DeserializeOwned>(
        &mut self,
        mut req: handler::EdgeReq<'s, O>,
        page: NonZeroUsize,
        f: impl FnOnce(EdgeIter<O>) -> Result<T, E>,
    ) -> Result<(T, ApiResponse<&'s str, HttpRequest>), Error<E>> {
        let _span = tracing::info_span!(
            "edge_request",
            ty = tracing::field::debug(&req.ty),
            start_page = page
        )
        .entered();

        let mut has_more = true;
        let ret = f(EdgeIter {
            client: &mut self.client,
            has_more: &mut has_more,
            page,
            request: &mut req,
        })
        .map_err(Error::Inner)?;

        let resp = req.finish(self.seq, page.get() == 1 && !has_more);
        self.seq += 1;
        self.sink.add_response(&resp).map_err(Error::Sink)?;
        Ok((ret, resp))
    }

    pub fn fetch_revision<'s>(
        &mut self,
        req: handler::RevisionReq<'s>,
    ) -> Result<client::RevisionData<'s>, Error<Infallible>> {
        let _span =
            tracing::info_span!("revision_request", ty = tracing::field::debug(&req.ty)).entered();
        let ret = self
            .client
            .request_revision(self.seq, req)
            .map_err(Error::Client)?;
        self.seq += 1;
        self.sink.add_response(&ret.response).map_err(Error::Sink)?;
        Ok(ret)
    }
}
