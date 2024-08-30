use std::io::{self, Write};

use webar_data::{cbor, ser::Serialize};
use webar_wordpress_core::fetcher::rest_client::{ApiResponse, HttpRequest};

pub use io::Error;

fn add_file<W: Write, A: Serialize, R: Serialize>(
    file: &mut tar::Builder<W>,
    resp: &ApiResponse<A, R>,
) -> io::Result<()> {
    let data = cbor::to_vec(resp);
    let mut header = tar::Header::new_gnu();
    header.set_mode(0o444);
    header.set_size(data.len() as u64);
    file.append_data(&mut header, format!("{}.bin", resp.seq), data.as_slice())
}

pub struct TarSink<MW: Write, RW: Write> {
    meta: tar::Builder<MW>,
    response: tar::Builder<RW>,
}
impl<MW: Write, RW: Write> TarSink<MW, RW> {
    pub fn new(meta: MW, response: RW) -> Self {
        Self {
            meta: tar::Builder::new(meta),
            response: tar::Builder::new(response),
        }
    }
    pub fn add_response<A: Copy + Serialize>(
        &mut self,
        resp: &ApiResponse<A, HttpRequest>,
    ) -> Result<(), Error> {
        add_file(&mut self.meta, &resp.to_meta())?;
        add_file(&mut self.response, resp)
    }
    pub fn into_inner(self) -> Result<(MW, RW), Error> {
        Ok((self.meta.into_inner()?, self.response.into_inner()?))
    }
}
