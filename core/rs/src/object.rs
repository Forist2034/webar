use std::{fmt::Debug, marker::PhantomData};

use serde::Deserialize;
use webar_data::ser::Serialize;

use crate::digest::Digest;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct DataId(pub Digest);

pub struct ObjectId<T>(pub Digest, pub PhantomData<T>);
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
