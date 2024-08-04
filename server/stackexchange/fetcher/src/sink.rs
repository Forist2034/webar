use std::{io, path::Path};

use webar_data::{
    cbor, json,
    ser::{Never, Serialize},
};

use webar_stackexchange_core::{
    api::request::ResponseData,
    fetcher::api_client::{ApiResponse, ListData, ObjectsData},
};

pub use io::Error;

fn add_file<W: io::Write>(
    file: &mut tar::Builder<W>,
    path: impl AsRef<Path>,
    data: &[u8],
) -> Result<(), Error> {
    let mut header = tar::Header::new_gnu();
    header.set_mode(0o444);
    header.set_size(data.len() as u64);
    file.append_data(&mut header, path, data)
}

pub struct TarSink<MW: io::Write, RW: io::Write> {
    meta: tar::Builder<MW>,
    meta_json: tar::Builder<MW>,
    response: tar::Builder<RW>,
}
impl<MW: io::Write, RW: io::Write> TarSink<MW, RW> {
    pub fn new(meta: MW, meta_json: MW, response: RW) -> Self {
        Self {
            meta: tar::Builder::new(meta),
            meta_json: tar::Builder::new(meta_json),
            response: tar::Builder::new(response),
        }
    }

    fn add_meta<O: Serialize>(&mut self, meta: &ApiResponse<O>) -> Result<(), Error> {
        add_file(
            &mut self.meta,
            format!("{}.bin", meta.seq),
            &cbor::to_vec(meta),
        )?;
        add_file(
            &mut self.meta_json,
            format!("{}.json", meta.seq),
            &json::to_vec(meta).unwrap(),
        )
    }
    fn add_response<O: Serialize>(&mut self, response: &ApiResponse<O>) -> Result<(), Error> {
        add_file(
            &mut self.response,
            format!("{}.bin", response.seq),
            &cbor::to_vec(response),
        )
    }

    pub fn add_objects(&mut self, response: &ApiResponse<ObjectsData>) -> Result<(), Error> {
        self.add_meta(&ApiResponse {
            site: response.site,
            seq: response.seq,
            api_version: response.api_version,
            filter: response.filter,
            data: ResponseData::Objects::<_, Never>(response.data.to_meta()),
        })?;
        self.add_response(&ApiResponse {
            site: response.site,
            seq: response.seq,
            api_version: response.api_version,
            filter: response.filter,
            data: ResponseData::Objects::<_, Never>(&response.data),
        })
    }

    pub fn add_lists<S: Serialize>(
        &mut self,
        response: &ApiResponse<ListData<S>>,
    ) -> Result<(), Error> {
        self.add_meta(&ApiResponse {
            site: response.site,
            seq: response.seq,
            api_version: response.api_version,
            filter: response.filter,
            data: ResponseData::List::<Never, _>(response.data.to_meta()),
        })?;
        self.add_response(&ApiResponse {
            site: response.site,
            seq: response.seq,
            api_version: response.api_version,
            filter: response.filter,
            data: ResponseData::List::<Never, _>(&response.data),
        })
    }

    pub fn into_inner(self) -> Result<(MW, RW), Error> {
        Ok((self.meta.into_inner()?, self.response.into_inner()?))
    }
}
