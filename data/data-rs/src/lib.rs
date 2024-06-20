pub mod ser;

pub mod cbor;

pub mod json;

#[doc(hidden)]
pub mod __internal {
    pub use std::result::Result;
}

pub mod bytes {
    use serde::Deserialize;

    use crate::ser::{Serialize, Serializer};

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ByteBuf(pub Vec<u8>);
    impl Serialize for ByteBuf {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            serializer.serialize_bytes(self.0.as_slice())
        }
    }
    impl<'de> Deserialize<'de> for ByteBuf {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct Visitor;
            impl<'de> serde::de::Visitor<'de> for Visitor {
                type Value = ByteBuf;
                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str("byte buf")
                }
                fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(ByteBuf(Vec::from(v)))
                }
                fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(ByteBuf(v))
                }
            }
            deserializer.deserialize_byte_buf(Visitor)
        }
    }

    #[derive(Debug, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    pub struct Bytes(pub [u8]);
    impl Bytes {
        pub const fn new(data: &[u8]) -> &Self {
            unsafe { &*(data as *const [u8] as *const Bytes) }
        }
        pub fn as_bytes(&self) -> &[u8] {
            &self.0
        }
    }
    impl Serialize for Bytes {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            serializer.serialize_bytes(&self.0)
        }
    }
    impl<'de> Deserialize<'de> for &'de Bytes {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            struct Visitor;
            impl<'de> serde::de::Visitor<'de> for Visitor {
                type Value = &'de Bytes;
                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    formatter.write_str("bytes")
                }
                fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(Bytes::new(v))
                }
            }
            deserializer.deserialize_bytes(Visitor)
        }
    }
}
