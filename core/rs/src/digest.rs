use std::fmt::{Debug, Display};

use serde::Deserialize;

use webar_data::ser::Serialize;

mod serde_digest {
    use serde::{de::Visitor, Deserializer};

    use webar_data::ser::Serializer;

    pub fn serialize<const BUF: usize, const N: usize, S: Serializer>(
        data: &[u8; N],
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        if S::HUMAN_READABLE {
            let mut buf = [0; BUF];
            const_hex::encode_to_slice(data, &mut buf).unwrap();
            serializer.serialize_str(unsafe { std::str::from_utf8_unchecked(&buf) })
        } else {
            serializer.serialize_bytes(data)
        }
    }
    pub fn deserialize<'de, const N: usize, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<[u8; N], D::Error> {
        if deserializer.is_human_readable() {
            const_hex::deserialize(deserializer)
        } else {
            struct Vis<const N: usize>;
            impl<'de, const N: usize> Visitor<'de> for Vis<N> {
                type Value = [u8; N];
                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(formatter, "{} bytes binary digest", N)
                }
                fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    match <[u8; N]>::try_from(v) {
                        Ok(v) => Ok(v),
                        Err(_) => Err(E::invalid_length(v.len(), &self)),
                    }
                }
            }
            deserializer.deserialize_bytes(Vis)
        }
    }
}

fn show_hash(d: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for i in d {
        write!(f, "{i:02x}")?;
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sha256(pub [u8; 32]);
impl Sha256 {
    pub fn digest(data: impl AsRef<[u8]>) -> Self {
        use sha2::Digest;
        Self(sha2::Sha256::digest(data).into())
    }
}
impl Debug for Sha256 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        show_hash(&self.0, f)
    }
}
impl Display for Sha256 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        show_hash(&self.0, f)
    }
}
impl Serialize for Sha256 {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serde_digest::serialize::<64, 32, _>(&self.0, serializer)
    }
}
impl<'de> serde::Deserialize<'de> for Sha256 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_digest::deserialize(deserializer).map(Self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Digest {
    #[serde(rename = "sha256")]
    Sha256(Sha256),
}
impl Digest {
    /// hash data with default algorithm
    pub fn digest(data: &[u8]) -> Self {
        Self::Sha256(Sha256::digest(data))
    }
}
impl From<Sha256> for Digest {
    fn from(value: Sha256) -> Self {
        Self::Sha256(value)
    }
}
