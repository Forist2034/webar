use std::{any::type_name, fmt::Display, time::SystemTime};

use codec::cbor::internal::{decoding, encoding};
use serde::Deserialize;

use webar_data::ser::Serialize;

pub mod bytes;

pub mod codec {
    pub mod cbor;
}

pub mod digest;

pub mod http;

pub mod blob;

pub mod time;

pub mod object;

pub mod fetch;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Timestamp {
    pub secs: u64,
    pub nanos: u32,
}
impl Timestamp {
    pub fn now() -> Self {
        Self::from(SystemTime::now())
    }
}
impl From<SystemTime> for Timestamp {
    fn from(value: SystemTime) -> Self {
        let d = value.duration_since(SystemTime::UNIX_EPOCH).unwrap();
        Self {
            secs: d.as_secs(),
            nanos: d.subsec_nanos(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Version(pub u8, pub u8);
impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}
impl encoding::ToCbor for Version {
    fn encode<'a, W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<'a, W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        let mut enc = encoder.encode_tuple_struct(type_name::<Self>(), 2)?;
        enc.encode_field(&self.0)?;
        enc.encode_field(&self.1)?;
        enc.end()
    }
}
impl<R: decoding::Read> decoding::FromCbor<R> for Version {
    fn decode<'a>(
        decoder: decoding::Decoder<'a, R>,
    ) -> Result<Self, decoding::Error<<R as ciborium_io::Read>::Error>> {
        let mut dec = decoder.decode_tuple_struct_len(type_name::<Self>(), 2)?;
        Ok(Self(dec.next_field()?, dec.next_field()?))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Server<N> {
    pub name: N,
    pub version: Version,
}
impl<N: Serialize> Serialize for Server<N> {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use webar_data::ser::SerializeTupleStruct;
        let mut ser = serializer.serialize_tuple_struct(2)?;
        ser.serialize_field(&self.name)?;
        ser.serialize_field(&self.version)?;
        ser.end()
    }
}
impl<N: encoding::ToCbor> encoding::ToCbor for Server<N> {
    fn encode<'a, W: ciborium_io::Write>(
        &self,
        encoder: codec::cbor::internal::encoding::Encoder<'a, W>,
    ) -> Result<(), codec::cbor::internal::encoding::Error<W::Error>> {
        let mut enc = encoder.encode_tuple_struct(type_name::<Self>(), 2)?;
        enc.encode_field(&self.name)?;
        enc.encode_field(&self.version)?;
        enc.end()
    }
}
impl<R: decoding::Read, N: decoding::FromCbor<R>> decoding::FromCbor<R> for Server<N> {
    fn decode<'a>(
        decoder: decoding::Decoder<'a, R>,
    ) -> Result<Self, decoding::Error<<R as ciborium_io::Read>::Error>> {
        let mut dec = decoder.decode_tuple_struct_len(type_name::<Self>(), 2)?;
        Ok(Self {
            name: dec.next_field()?,
            version: dec.next_field()?,
        })
    }
}

pub struct FilePath {
    pub path: &'static str,
    pub c_path: &'static std::ffi::CStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Domain<D>(pub D);
impl<D: Display> Display for Domain<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

// TODO: support ip address when ip addr serialize is implemented
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Host<S> {
    #[serde(rename = "domain")]
    Domain(Domain<S>),
}
