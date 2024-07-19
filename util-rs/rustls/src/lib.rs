use std::{
    fmt::Debug,
    io,
    sync::{Arc, Mutex},
};

use rustls::{ClientConfig, KeyLog, RootCertStore};

#[derive(Debug)]
pub struct WriteLog<W>(Mutex<W>);

impl<W: io::Write> WriteLog<W> {
    pub fn new(writer: W) -> Self {
        Self(Mutex::new(writer))
    }
    fn try_write(&self, label: &str, client_random: &[u8], secret: &[u8]) -> Result<(), io::Error> {
        let mut f = self.0.lock().unwrap();
        write!(f, "{label} ")?;
        for b in client_random.iter() {
            write!(f, "{b:02x}")?;
        }
        write!(f, " ")?;
        for b in secret.iter() {
            write!(f, "{b:02x}")?;
        }
        writeln!(f)?;
        f.flush()
    }
}

impl<W: Debug + Send + io::Write> KeyLog for WriteLog<W> {
    fn log(&self, label: &str, client_random: &[u8], secret: &[u8]) {
        if let Err(e) = self.try_write(label, client_random, secret) {
            tracing::error!("failed to write key log: {e}");
        }
    }
    fn will_log(&self, _label: &str) -> bool {
        true
    }
}

fn new_cfg() -> ClientConfig {
    let mut ret = ClientConfig::builder()
        .with_root_certificates(Arc::new(RootCertStore {
            roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
        }))
        .with_no_client_auth();
    ret.enable_sni = true;
    ret
}
const HTTP_11: &[u8] = b"http/1.1";
const HTTP2: &[u8] = b"h2";

pub fn global_init() {
    rustls::crypto::aws_lc_rs::default_provider()
        .install_default()
        .unwrap()
}

pub fn default_config<W: Debug + io::Write + Send + 'static>(keylog: W) -> ClientConfig {
    let mut ret = new_cfg();
    ret.alpn_protocols = Vec::from([HTTP_11.to_vec(), HTTP2.to_vec()]);
    ret.key_log = Arc::new(WriteLog::new(keylog));
    ret
}
