use std::{io, path::Path};

use webar_data::{cbor, ser::Serialize};

use webar_stackexchange_core::fetcher::rest_client::{ApiResponse, HttpRequest};

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
    response: tar::Builder<RW>,
}
impl<MW: io::Write, RW: io::Write> TarSink<MW, RW> {
    pub fn new(meta: MW, response: RW) -> Self {
        Self {
            meta: tar::Builder::new(meta),
            response: tar::Builder::new(response),
        }
    }

    pub fn add_response<S: Copy + Serialize>(
        &mut self,
        response: &ApiResponse<S, HttpRequest>,
    ) -> Result<(), Error> {
        let path = format!("{}.bin", response.seq);
        add_file(&mut self.response, &path, &cbor::to_vec(response))?;
        add_file(&mut self.meta, &path, &cbor::to_vec(&response.to_meta()))
    }

    pub fn into_inner(self) -> Result<(MW, RW), Error> {
        Ok((self.meta.into_inner()?, self.response.into_inner()?))
    }
}
