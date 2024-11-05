use std::{
    any::type_name,
    collections::{BTreeMap, BTreeSet},
};

pub use ciborium_io::Write;
use ciborium_ll::Header;
use uuid::Uuid;

use crate::bytes::ByteBuf;

use super::{TypeInfo, ENUM_TAG, UUID_TAG};

#[derive(Debug, thiserror::Error)]
pub struct Error<E>(#[from] pub(in crate::codec::cbor) E);

pub struct Encoder<'a, W: Write>(pub(in crate::codec::cbor) &'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> Encoder<'a, W> {
    pub fn encode_tuple_struct(
        self,
        _ty: TypeInfo,
        size: usize,
    ) -> Result<TupleStructEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Array(Some(size)))?;
        Ok(TupleStructEncoder(self.0))
    }
    pub fn encode_struct(
        self,
        _ty: TypeInfo,
        size: usize,
    ) -> Result<StructEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Map(Some(size)))?;
        Ok(StructEncoder(self.0))
    }

    pub fn encode_unit_variant(
        self,
        _ty: TypeInfo,
        variant: &'static str,
    ) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Text(Some(variant.len())))?;
        self.0.write_all(variant.as_bytes())?;
        Ok(())
    }
    pub fn encode_tuple_variant(
        self,
        _ty: TypeInfo,
        variant: &'static str,
        size: usize,
    ) -> Result<TupleVariantEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Tag(ENUM_TAG))?;
        self.0.push(Header::Array(Some(2)))?;

        self.0.push(Header::Text(Some(variant.len())))?;
        self.0.write_all(variant.as_bytes())?;

        self.0.push(Header::Array(Some(size)))?;
        Ok(TupleVariantEncoder(self.0))
    }
    pub fn encode_struct_variant(
        self,
        _ty: TypeInfo,
        variant: &'static str,
        size: usize,
    ) -> Result<StructVariantEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Tag(ENUM_TAG))?;
        self.0.push(Header::Array(Some(2)))?;

        self.0.push(Header::Text(Some(variant.len())))?;
        self.0.write_all(variant.as_bytes())?;

        self.0.push(Header::Map(Some(size)))?;
        Ok(StructVariantEncoder(self.0))
    }

    pub fn encode_list(
        self,
        _ty: TypeInfo,
        len: usize,
    ) -> Result<ListEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Array(Some(len)))?;
        Ok(ListEncoder(self.0))
    }
    pub fn encode_map(
        self,
        _ty: TypeInfo,
        len: usize,
    ) -> Result<MapEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Map(Some(len)))?;
        Ok(MapEncoder(self.0))
    }
}

pub struct TupleStructEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> TupleStructEncoder<'a, W> {
    pub fn encode_field<D: ToCbor>(&mut self, value: &D) -> Result<(), Error<W::Error>> {
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct StructEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> StructEncoder<'a, W> {
    pub fn encode_field<D: ToCbor>(
        &mut self,
        key: &'static str,
        value: &D,
    ) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Text(Some(key.len())))?;
        self.0.write_all(key.as_bytes())?;
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct TupleVariantEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> TupleVariantEncoder<'a, W> {
    pub fn encode_field<D: ToCbor>(&mut self, value: &D) -> Result<(), Error<W::Error>> {
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct StructVariantEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> StructVariantEncoder<'a, W> {
    pub fn encode_field<D: ToCbor>(
        &mut self,
        key: &'static str,
        value: &D,
    ) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Text(Some(key.len())))?;
        self.0.write_all(key.as_bytes())?;
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct ListEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> ListEncoder<'a, W> {
    pub fn encode_element<D: ToCbor>(&mut self, value: &D) -> Result<(), Error<W::Error>> {
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct MapEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> MapEncoder<'a, W> {
    pub fn encode_entry<K: ToCbor, V: ToCbor>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Error<W::Error>> {
        key.encode(Encoder(&mut *self.0))?;
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub trait ToCbor {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>>;
}
impl ToCbor for bool {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(Header::Simple(if *self {
                ciborium_ll::simple::TRUE
            } else {
                ciborium_ll::simple::FALSE
            }))
            .map_err(Error)
    }
}
impl ToCbor for u8 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToCbor for u16 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToCbor for u32 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToCbor for u64 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Positive(*self)).map_err(Error)
    }
}
impl ToCbor for i8 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToCbor for i16 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToCbor for i32 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToCbor for i64 {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(if self.is_negative() {
                Header::Negative((*self as u64) ^ !0)
            } else {
                Header::Positive(*self as u64)
            })
            .map_err(Error)
    }
}
impl ToCbor for () {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(Header::Simple(ciborium_ll::simple::NULL))
            .map_err(Error)
    }
}

impl ToCbor for str {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Text(Some(self.len())))?;
        encoder.0.write_all(self.as_bytes()).map_err(Error)
    }
}
impl ToCbor for String {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        self.as_str().encode(encoder)
    }
}

impl ToCbor for ByteBuf {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Bytes(Some(self.0.len())))?;
        encoder.0.write_all(&self.0).map_err(Error)
    }
}

impl<I: ToCbor> ToCbor for Option<I> {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        match self {
            Some(v) => v.encode(encoder),
            None => encoder
                .0
                .push(Header::Simple(ciborium_ll::simple::NULL))
                .map_err(Error),
        }
    }
}

impl<const N: usize, I: ToCbor> ToCbor for [I; N] {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        let mut seq = encoder.encode_list(type_name::<Self>(), N)?;
        for i in self {
            seq.encode_element(i)?;
        }
        seq.end()
    }
}
impl<I: ToCbor> ToCbor for [I] {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        let mut seq = encoder.encode_list(type_name::<Self>(), self.len())?;
        for i in self {
            seq.encode_element(i)?;
        }
        seq.end()
    }
}
impl<I: ToCbor> ToCbor for Vec<I> {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        self.as_slice().encode(encoder)
    }
}

impl<'b, T: ?Sized + ToCbor> ToCbor for &'b T {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        T::encode(*self, encoder)
    }
}
impl<T: ?Sized + ToCbor> ToCbor for Box<T> {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        T::encode(&self, encoder)
    }
}

impl<I: ToCbor> ToCbor for BTreeSet<I> {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        let mut seq = encoder.encode_list(type_name::<Self>(), self.len())?;
        for i in self {
            seq.encode_element(i)?;
        }
        seq.end()
    }
}
impl<K: ToCbor, V: ToCbor> ToCbor for BTreeMap<K, V> {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        let mut enc = encoder.encode_map(type_name::<Self>(), self.len())?;
        for (k, v) in self {
            enc.encode_entry(k, v)?;
        }
        enc.end()
    }
}

impl ToCbor for Uuid {
    fn encode<'a, W: Write>(&self, encoder: Encoder<'a, W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Tag(UUID_TAG))?;
        encoder.0.push(Header::Bytes(Some(16)))?;
        encoder.0.write_all(self.as_bytes()).map_err(Error)
    }
}
