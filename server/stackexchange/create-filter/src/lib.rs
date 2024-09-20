use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use reqwest::blocking::Client;
use serde::Deserialize;

use webar_data::bytes::ByteBuf;
use webar_stackexchange_core::rest_api::model;

#[derive(Debug)]
pub enum CreateError {
    Http(reqwest::Error),
    Json(serde_json::Error),
}
impl Display for CreateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Http(e) => write!(f, "http error: {e}"),
            Self::Json(e) => write!(f, "failed to decode json: {e}"),
        }
    }
}
impl std::error::Error for CreateError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Http(e) => Some(e),
            Self::Json(e) => Some(e),
        }
    }
}

#[derive(Deserialize)]
#[serde(bound = "'de:'a")]
pub struct FilterConfig<'a> {
    pub safe: bool,
    #[serde(default)]
    pub base: Option<&'a str>,
    pub wrapper: HashSet<&'a str>,
    pub item: HashMap<&'a str, HashSet<&'a str>>,
}

impl<'a> FilterConfig<'a> {
    fn included_param(&self) -> String {
        use std::fmt::Write;
        let mut ret = String::new();
        let mut it = self
            .wrapper
            .iter()
            .map(|f| ("", *f))
            .chain(
                self.item
                    .iter()
                    .flat_map(|(t, tf)| tf.iter().map(|f| (*t, *f))),
            )
            .fuse();
        if let Some((t, f)) = it.next() {
            write!(ret, "{t}.{f}").unwrap();
        }
        for (t, f) in it {
            write!(ret, ";{t}.{f}").unwrap();
        }
        ret
    }
    pub fn create(&self, client: &Client) -> Result<(ByteBuf, model::Filter<String>), CreateError> {
        let bs: Vec<u8> = client
            .get("https://api.stackexchange.com/2.3/filters/create")
            .query(&[("include", self.included_param())])
            .query(&[("base", self.base)])
            .query(&[("unsafe", !self.safe)])
            .send()
            .map_err(CreateError::Http)?
            .error_for_status()
            .map_err(CreateError::Http)?
            .bytes()
            .map_err(CreateError::Http)?
            .into();
        let v = match serde_json::from_slice::<model::Wrapper<[model::Filter<String>; 1]>>(&bs) {
            Ok(model::Wrapper { items: [v], .. }) => v,
            Err(e) => return Err(CreateError::Json(e)),
        };
        Ok((ByteBuf(bs), v))
    }
}
