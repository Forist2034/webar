use std::{
    convert::Infallible, fmt::Debug, io, iter::FusedIterator, num::NonZeroUsize, thread::sleep,
    time::Duration,
};

use webar_data::ser::Serialize;
use webar_stackexchange_core::{
    api,
    fetcher::api_client::{ApiData, ApiResponse, ListData, ObjectsData},
    NonEmpty,
};

pub mod client;
pub use client::Client;

pub mod sink;

pub struct ManyChunk<const N: usize, I>(pub I);
impl<const N: usize, I: FusedIterator> Iterator for ManyChunk<N, I> {
    type Item = NonEmpty<I::Item, Vec<I::Item>>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(NonEmpty(self.0.next()?, self.0.by_ref().take(N).collect()))
    }
}

fn wait_backoff(backoff: Option<u64>) {
    if let Some(b) = backoff {
        tracing::info!(backoff = b, "received backoff");
        sleep(Duration::from_secs(b));
    }
}

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

pub struct Fetcher<MW: io::Write, RW: io::Write, FN> {
    pub client: Client<FN>,
    pub sink: sink::TarSink<MW, RW>,
}

pub struct ListIter<'a, FN, E: client::ListApi> {
    request: &'a mut client::ListReq<E, E::Str>,
    client: &'a mut client::Client<FN>,
    has_more: &'a mut bool,
    page: usize,
}
impl<'a, FN, E> Iterator for ListIter<'a, FN, E>
where
    FN: AsRef<str>,
    E: client::ListApi,
{
    type Item = Result<api::Wrapper<Vec<E::Output>>, client::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if *self.has_more {
            let _span = tracing::info_span!("fetch_page", page = self.page).entered();
            let ret = self.client.request_list(&mut self.request, self.page);
            if let Ok(ref o) = ret {
                wait_backoff(o.backoff);
                *self.has_more = o.has_more;
                self.page += 1;
            }
            Some(ret)
        } else {
            None
        }
    }
}
impl<'a, E, FN> FusedIterator for ListIter<'a, FN, E>
where
    FN: AsRef<str>,
    E: client::ListApi,
{
}

impl<MW, RW, FN> Fetcher<MW, RW, FN>
where
    MW: io::Write,
    RW: io::Write,
    FN: AsRef<str>,
{
    pub fn fetch_object<R: client::ObjectApi>(
        &mut self,
        request: R,
    ) -> Result<ApiData<ObjectsData, R::Output>, Error<Infallible>> {
        let _entered = tracing::info_span!("objects_request", "type" = R::TYPE.as_str()).entered();
        let ret = self.client.request_object(request).map_err(Error::Client)?;
        self.sink.add_objects(&ret.response).map_err(Error::Sink)?;
        Ok(ret)
    }

    pub fn with_list_iter<R: client::ListApi, T, E>(
        &mut self,
        request: R,
        page: NonZeroUsize,
        f: impl FnOnce(ListIter<'_, FN, R>) -> Result<T, E>,
    ) -> Result<(T, ApiResponse<ListData<R::Str>>), Error<E>>
    where
        R::Str: Debug + Serialize,
    {
        let mut request = request.into_req();

        let _span = tracing::info_span!(
            "list_request",
            request = tracing::field::debug(&request.request),
            start_page = page
        )
        .entered();

        let mut has_more = true;
        let ret = f(ListIter {
            client: &mut self.client,
            has_more: &mut has_more,
            page: page.get(),
            request: &mut request,
        })
        .map_err(Error::Inner)?;

        let resp = self
            .client
            .finish_list(request, page.get() == 1 && has_more);
        self.sink.add_lists(&resp).map_err(Error::Sink)?;
        Ok((ret, resp))
    }
}
