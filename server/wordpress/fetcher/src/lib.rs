pub mod client;
use std::{convert::Infallible, io::Write, iter::FusedIterator};

pub use client::{Client, Handler};
use serde::de::DeserializeOwned;

pub mod sink;

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("client error")]
    Client(#[source] client::Error),
    #[error("sink error")]
    Sink(#[source] sink::Error),
    #[error("inner error")]
    Inner(#[source] E),
}

pub struct Fetcher<MW: Write, RW: Write> {
    pub seq: u32,
    pub client: client::Client,
    pub sink: sink::TarSink<MW, RW>,
}
pub struct PagedData<O> {
    pub offset: usize,
    pub data: Vec<O>,
    pub paging: client::Paging,
}
pub struct PageIter<'a, 's, 'h, O> {
    req: &'a mut client::PageReq<'s, 'h, O>,
    client: &'a client::Client,
    has_more: &'a mut bool,
    offset: usize,
}
impl<'a, 's, 'h, O: DeserializeOwned> Iterator for PageIter<'a, 's, 'h, O> {
    type Item = Result<PagedData<O>, client::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if *self.has_more {
            let (data, paging) = match self.client.request_page(self.req, self.offset) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            let offset = self.offset;
            self.offset += data.len();
            *self.has_more = self.offset < paging.total;
            Some(Ok(PagedData {
                offset,
                data,
                paging,
            }))
        } else {
            None
        }
    }
}
impl<'a, 's, 'h, O: DeserializeOwned> FusedIterator for PageIter<'a, 's, 'h, O> {}

impl<MW: Write, RW: Write> Fetcher<MW, RW> {
    pub fn fetch_node<O: DeserializeOwned>(
        &mut self,
        req: client::NodeReq<O>,
    ) -> Result<O, Error<Infallible>> {
        let _span =
            tracing::info_span!("fetch_node", node = tracing::field::debug(&req.ty)).entered();
        let seq = self.seq;
        self.seq += 1;
        let ret = self.client.request_node(seq, req).map_err(Error::Client)?;
        self.sink.add_response(&ret.response).map_err(Error::Sink)?;
        Ok(ret.parsed)
    }
    pub fn with_page_iter<T, E, O: DeserializeOwned>(
        &mut self,
        mut req: client::PageReq<O>,
        offset: usize,
        f: impl FnOnce(PageIter<O>) -> Result<T, E>,
    ) -> Result<T, Error<E>> {
        let _span =
            tracing::info_span!("fetch_page", ty = tracing::field::debug(&req.ty)).entered();
        let mut has_more = true;
        let ret = f(PageIter {
            req: &mut req,
            client: &self.client,
            has_more: &mut has_more,
            offset,
        })
        .map_err(Error::Inner)?;
        let seq = self.seq;
        self.seq += 1;
        self.sink
            .add_response(&req.finish(seq, offset == 0 && !has_more))
            .map_err(Error::Sink)?;
        Ok(ret)
    }
}
