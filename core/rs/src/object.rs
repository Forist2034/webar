use std::{fmt::Debug, marker::PhantomData};

use serde::Deserialize;
use webar_data::ser::Serialize;

use crate::digest::Digest;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Server<N> {
    pub name: N,
    pub version: u8,
}
impl<N: AsRef<str>> Serialize for Server<N> {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use webar_data::ser::SerializeTupleStruct;
        let mut s = serializer.serialize_tuple_struct(2)?;
        s.serialize_field(self.name.as_ref())?;
        s.serialize_field(&self.version)?;
        s.end()
    }
}
impl<'de, N: Deserialize<'de>> Deserialize<'de> for Server<N> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct ServerTuple<N>(N, u8);
        ServerTuple::deserialize(deserializer).map(|t| Self {
            name: t.0,
            version: t.1,
        })
    }
}

pub struct ObjectId<T>(pub Digest, pub PhantomData<fn() -> T>);
impl<T> Debug for ObjectId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ObjectId").field(&self.0).finish()
    }
}
impl<T> Clone for ObjectId<T> {
    fn clone(&self) -> Self {
        ObjectId(self.0.clone(), PhantomData)
    }
}
impl<T> Copy for ObjectId<T> {}
impl<T> PartialEq for ObjectId<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Eq for ObjectId<T> {}
impl<T> PartialOrd for ObjectId<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T> Ord for ObjectId<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> Serialize for ObjectId<T> {
    #[inline]
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}
impl<'de, T> Deserialize<'de> for ObjectId<T> {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Digest::deserialize(deserializer).map(|d| ObjectId(d, PhantomData))
    }
}
impl<T> ObjectId<T> {
    #[inline]
    pub const fn new(digest: Digest) -> Self {
        Self(digest, PhantomData)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Snapshot:Serialize, Record:Serialize",
    deserialize = "Snapshot:Deserialize<'de>, Record:Deserialize<'de>"
))]
pub enum ObjectType<Archive, Snapshot, Record> {
    #[serde(rename = "snapshot")]
    Snapshot {
        archive: ObjectId<Archive>,
        #[serde(rename = "type")]
        ty: Snapshot,
    },
    #[serde(rename = "record")]
    Record(Record),
    #[serde(rename = "archive")]
    Archive,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Host:Serialize, Snapshot:Serialize, Record:Serialize",
    deserialize = "Host:Deserialize<'de>, Snapshot:Deserialize<'de>, Record:Deserialize<'de>"
))]
pub struct ObjectInfo<Host, Archive, Snapshot, Record> {
    pub host: Host,
    #[serde(rename = "type")]
    pub ty: ObjectType<Archive, Snapshot, Record>,
    pub version: u8,
}

pub fn encode_object<N: AsRef<str>, Host, Archive, Snapshot, Record, D>(
    server: &Server<N>,
    info: &ObjectInfo<Host, Archive, Snapshot, Record>,
    data: &D,
) -> Vec<u8>
where
    Host: Serialize,
    Archive: Serialize,
    Snapshot: Serialize,
    Record: Serialize,
    D: Serialize,
{
    struct Object<'a, S, O, D>(&'a S, &'a O, &'a D);
    impl<'a, S, O, D> Serialize for Object<'a, S, O, D>
    where
        S: Serialize,
        O: Serialize,
        D: Serialize,
    {
        fn serialize<Ser: webar_data::ser::Serializer>(
            &self,
            serializer: Ser,
        ) -> Result<Ser::Ok, Ser::Error> {
            use webar_data::ser::SerializeStruct;
            let mut ser = serializer.serialize_struct(4)?;
            ser.serialize_field("version", &1)?;
            ser.serialize_field("server", self.0)?;
            ser.serialize_field("info", self.1)?;
            ser.serialize_field("data", self.2)?;
            ser.end()
        }
    }
    webar_data::cbor::to_vec(&Object(server, info, data))
}
