use std::{collections::BTreeMap, str::FromStr};

use serde::Deserialize;
use webar_data::ser::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method(http::Method);

impl From<http::Method> for Method {
    fn from(value: http::Method) -> Self {
        Self(value)
    }
}
impl Serialize for Method {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.0.as_str())
    }
}
impl<'de> Deserialize<'de> for Method {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Method;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("http method")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match http::Method::from_str(v) {
                    Ok(m) => Ok(Method(m)),
                    Err(e) => Err(E::custom(e)),
                }
            }
        }
        deserializer.deserialize_str(Visitor)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatusCode(http::StatusCode);

impl From<http::StatusCode> for StatusCode {
    fn from(value: http::StatusCode) -> Self {
        Self(value)
    }
}
impl Serialize for StatusCode {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u16(self.0.as_u16())
    }
}
impl<'de> Deserialize<'de> for StatusCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = StatusCode;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("http status code")
            }
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match http::StatusCode::from_u16(v) {
                    Ok(c) => Ok(StatusCode(c)),
                    Err(e) => Err(E::custom(e)),
                }
            }
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match u16::try_from(v) {
                    Ok(u) => self.visit_u16(u),
                    Err(_) => {
                        return Err(E::invalid_value(serde::de::Unexpected::Unsigned(v), &self))
                    }
                }
            }
        }
        deserializer.deserialize_u16(Visitor)
    }
}

mod header;
pub use header::HeaderName;

use crate::Timestamp;

#[derive(Debug, Clone)]
pub struct HeaderValue(http::HeaderValue);
impl From<http::HeaderValue> for HeaderValue {
    fn from(value: http::HeaderValue) -> Self {
        Self(value)
    }
}
impl Serialize for HeaderValue {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self.0.to_str() {
            Ok(s) => serializer.serialize_str(s),
            Err(_) => serializer.serialize_bytes(self.0.as_bytes()),
        }
    }
}
impl<'de> Deserialize<'de> for HeaderValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = HeaderValue;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("header value bytes")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match http::HeaderValue::from_str(v) {
                    Ok(v) => Ok(HeaderValue(v)),
                    Err(e) => Err(E::custom(e)),
                }
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match http::HeaderValue::from_bytes(v) {
                    Ok(v) => Ok(HeaderValue(v)),
                    Err(e) => Err(E::custom(e)),
                }
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct HeaderMap<T>(pub BTreeMap<HeaderName, Vec<T>>);
impl From<http::HeaderMap<http::HeaderValue>> for HeaderMap<HeaderValue> {
    fn from(value: http::HeaderMap<http::HeaderValue>) -> Self {
        let mut ret = BTreeMap::new();
        for k in value.keys() {
            ret.insert(
                HeaderName::from_str(k.as_str()),
                value
                    .get_all(k)
                    .into_iter()
                    .map(|v| HeaderValue::from(v.to_owned()))
                    .collect(),
            );
        }
        Self(ret)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response<I, H, B> {
    pub id: I,
    pub timestamp: Timestamp,
    pub status: StatusCode,
    pub headers: H,
    pub body: B,
}
