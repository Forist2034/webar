use std::{fmt::Debug, marker::PhantomData};

use serde::de::Deserialize;

use webar_data::ser::Serialize;

use crate::digest::Digest;

#[doc(hidden)]
pub(crate) mod internal {
    pub trait BlobDataInner {
        const IS_IMAGE: bool;
    }
}

/// method is not stable now
pub trait BlobData: AsRef<[u8]> + internal::BlobDataInner {}

pub const fn is_image<D: BlobData>() -> bool {
    D::IS_IMAGE
}

pub struct ImageData<D>(pub D);
impl<D> internal::BlobDataInner for ImageData<D> {
    const IS_IMAGE: bool = true;
}
impl<D: AsRef<[u8]>> AsRef<[u8]> for ImageData<D> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}
impl<D: AsRef<[u8]>> BlobData for ImageData<D> {}

pub struct CborData<D, T>(pub D, pub PhantomData<T>);
impl<D, T> internal::BlobDataInner for CborData<D, T> {
    const IS_IMAGE: bool = false;
}
impl<D: AsRef<[u8]>, T> AsRef<[u8]> for CborData<D, T> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}
impl<D: AsRef<[u8]>, T> BlobData for CborData<D, T> {}

pub struct BlobId<T>(pub Digest, pub PhantomData<fn() -> T>);
impl<T> Debug for BlobId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("BlobId").field(&self.0).finish()
    }
}
impl<T> Clone for BlobId<T> {
    fn clone(&self) -> Self {
        BlobId(self.0.clone(), PhantomData)
    }
}
impl<T> Copy for BlobId<T> {}
impl<T> PartialEq for BlobId<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Eq for BlobId<T> {}
impl<T> PartialOrd for BlobId<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T> Ord for BlobId<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> Serialize for BlobId<T> {
    #[inline]
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}
impl<'de, T> Deserialize<'de> for BlobId<T> {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Digest::deserialize(deserializer).map(|d| BlobId(d, PhantomData))
    }
}

impl<T> BlobId<T> {
    #[inline]
    pub const fn new(digest: Digest) -> Self {
        Self(digest, PhantomData)
    }
}
