//! # Deterministic serialization
//! based on [serde](https://serde.rs/).
//!
//! Floating point is not supported due to complexity of making floating point
//! operation reproducible across different machine and languages.

use std::collections::{BTreeMap, BTreeSet};

pub trait SerializeList {
    type Ok;
    type Error;

    /// Serialize a sequence element.
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize;

    /// Finish serializing a sequence.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait SerializeTupleStruct {
    type Ok;
    type Error;

    /// Serialize a tuple struct field.
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize;

    /// Finish serializing a tuple struct.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait SerializeTupleVariant {
    type Ok;
    type Error;

    // Required methods
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize;
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait SerializeMap {
    type Ok;
    type Error;

    /// Serialize a map entry consisting of a key and a value.
    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Self::Error>
    where
        K: ?Sized + Serialize,
        V: ?Sized + Serialize;

    /// Finish serializing a map.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait SerializeStruct {
    type Ok;
    type Error;

    /// Serialize a struct field.
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize;

    /// Finish serializing a struct.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait SerializeStructVariant {
    type Ok;
    type Error;

    /// Serialize a struct variant field.
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize;

    /// Finish serializing a struct variant.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait Serializer: Sized {
    type Ok;
    type Error;

    type SerializeList: SerializeList<Ok = Self::Ok, Error = Self::Error>;
    type SerializeTupleStruct: SerializeTupleStruct<Ok = Self::Ok, Error = Self::Error>;
    type SerializeTupleVariant: SerializeTupleVariant<Ok = Self::Ok, Error = Self::Error>;
    type SerializeMap: SerializeMap<Ok = Self::Ok, Error = Self::Error>;
    type SerializeStruct: SerializeStruct<Ok = Self::Ok, Error = Self::Error>;
    type SerializeStructVariant: SerializeStructVariant<Ok = Self::Ok, Error = Self::Error>;

    const HUMAN_READABLE: bool;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error>;

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error>;
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error>;
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error>;
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error>;

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error>;
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error>;
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error>;
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error>;

    // fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error>;
    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error>;
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error>;

    fn serialize_none(self) -> Result<Self::Ok, Self::Error>;
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize;

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error>;
    // fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error>;
    fn serialize_unit_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error>;
    /* fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize<Self>; */
    fn serialize_newtype_variant<T>(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize;

    fn serialize_tuple_struct(
        self,
        // name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error>;
    fn serialize_tuple_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error>;
    fn serialize_struct(
        self,
        // name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error>;
    fn serialize_struct_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error>;

    fn serialize_map(self, len: usize) -> Result<Self::SerializeMap, Self::Error>;
    fn serialize_list(self, len: usize) -> Result<Self::SerializeList, Self::Error>;
}

pub trait Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

pub use webar_derive::Serialize;

macro_rules! prim_impl {
    ($t:ty, $f:ident) => {
        impl Serialize for $t {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                serializer.$f(*self)
            }
        }
    };
}

prim_impl!(bool, serialize_bool);

prim_impl!(u8, serialize_u8);
prim_impl!(u16, serialize_u16);
prim_impl!(u32, serialize_u32);
prim_impl!(u64, serialize_u64);

prim_impl!(i8, serialize_i8);
prim_impl!(i16, serialize_i16);
prim_impl!(i32, serialize_i32);
prim_impl!(i64, serialize_i64);

/// Never type, use std `!` when stabilized
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Never {}

impl Serialize for Never {
    fn serialize<S: Serializer>(&self, _: S) -> Result<S::Ok, S::Error> {
        match *self {}
    }
}
impl<'de> serde::de::Deserialize<'de> for Never {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        Err(D::Error::custom("never type should not exist"))
    }
}

impl Serialize for () {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_unit()
    }
}

impl Serialize for str {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self)
    }
}
impl Serialize for String {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<I: Serialize> Serialize for Option<I> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Some(v) => serializer.serialize_some(v),
            None => serializer.serialize_none(),
        }
    }
}

impl<const N: usize, I> Serialize for [I; N]
where
    I: Serialize,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_list(N)?;
        for i in self {
            seq.serialize_element(i)?;
        }
        seq.end()
    }
}
impl<I: Serialize> Serialize for Vec<I> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_list(self.len())?;
        for i in self {
            seq.serialize_element(i)?;
        }
        seq.end()
    }
}
impl<I: Serialize> Serialize for BTreeSet<I> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_list(self.len())?;
        for i in self {
            seq.serialize_element(i)?;
        }
        seq.end()
    }
}
impl<K: Serialize, V: Serialize> Serialize for BTreeMap<K, V> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut ser = serializer.serialize_map(self.len())?;
        for (k, v) in self.iter() {
            ser.serialize_entry(k, v)?;
        }
        ser.end()
    }
}

impl Serialize for uuid::Uuid {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if S::HUMAN_READABLE {
            let mut buf = uuid::Uuid::encode_buffer();
            serializer.serialize_str(self.as_hyphenated().encode_lower(&mut buf))
        } else {
            serializer.serialize_bytes(self.as_bytes())
        }
    }
}

impl<'a, T: ?Sized + Serialize> Serialize for &'a T {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        T::serialize(self, serializer)
    }
}
