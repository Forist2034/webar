use std::{
    any::type_name,
    collections::{BTreeMap, BTreeSet},
    mem::MaybeUninit,
    ptr,
};

pub use ciborium_io::Read;
use ciborium_ll::{simple, Header};

use crate::bytes::ByteBuf;

use super::TypeInfo;

#[derive(Debug, thiserror::Error)]
#[error("can't read {read_size} bytes from {remaining} bytes")]
pub struct ReadError {
    remaining: usize,
    read_size: usize,
}
pub struct Reader<'a>(&'a [u8]);
impl<'a> Reader<'a> {
    pub(in crate::codec::cbor) fn new(data: &'a [u8]) -> Self {
        Self(data)
    }
}
impl<'a> Read for Reader<'a> {
    type Error = ReadError;
    fn read_exact(&mut self, data: &mut [u8]) -> Result<(), Self::Error> {
        if self.0.len() >= data.len() {
            let (h, t) = self.0.split_at(data.len());
            data.copy_from_slice(h);
            self.0 = t;
            Ok(())
        } else {
            Err(ReadError {
                read_size: data.len(),
                remaining: self.0.len(),
            })
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum InnerError<E> {
    #[error("cbor error")]
    Cbor(#[source] ciborium_ll::Error<E>),
    #[error("{ty}: header type mismatch {actual:?}")]
    TypeError { ty: TypeInfo, actual: Header },
    #[error("invalid utf8: {buf:x?}")]
    Utf8Error {
        buf: Vec<u8>,
        #[source]
        source: std::str::Utf8Error,
    },
    #[error("{ty}: size {actual} does not match expected {expect}")]
    SizeMismatch {
        ty: TypeInfo,
        actual: usize,
        expect: usize,
    },
    #[error("{ty}: unknown field {field:?}")]
    UnknownField { ty: TypeInfo, field: String },
    #[error("{ty}: invalid header {header:?}, {expected} expected")]
    InvalidEnum {
        ty: TypeInfo,
        expected: &'static str,
        header: Header,
    },
    #[error("{ty}: unknown variant {variant:?}")]
    UnknownVariant { ty: TypeInfo, variant: String },
    #[error("{ty}::{variant}: variant body size {actual} does not match expected {expect}")]
    VariantSizeMismatch {
        ty: TypeInfo,
        variant: &'static str,
        actual: usize,
        expect: usize,
    },
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error<E>(Box<InnerError<E>>);
#[doc(hidden)]
impl<E> Error<E> {
    fn prim_type_error<T: ?Sized>(h: Header) -> Self {
        Self::from(InnerError::TypeError {
            ty: type_name::<T>(),
            actual: h,
        })
    }
    fn io(source: E) -> Self {
        Self::from(InnerError::Cbor(ciborium_ll::Error::Io(source)))
    }

    pub fn unknown_enum_variant<R: Read>(ty: TypeInfo, variant: Enum<R>) -> Self {
        Self::from(InnerError::UnknownVariant {
            ty,
            variant: match variant {
                Enum::Unit(v) => v,
                Enum::Struct(v, _, _) => v,
                Enum::Tuple(v, _, _) => v,
            }
            .to_string(),
        })
    }
    pub fn variant_size_mismatch(
        ty: TypeInfo,
        variant: &'static str,
        actual: usize,
        expect: usize,
    ) -> Self {
        Self::from(InnerError::VariantSizeMismatch {
            ty,
            variant,
            actual,
            expect,
        })
    }
}
impl<E> From<InnerError<E>> for Error<E> {
    fn from(value: InnerError<E>) -> Self {
        Self(Box::new(value))
    }
}

fn read_string<R: Read>(
    mut buf: Vec<u8>,
    len: usize,
    reader: &mut R,
) -> Result<String, Error<R::Error>> {
    let old_len = buf.len();
    buf.resize(len, 0);
    reader.read_exact(&mut buf[old_len..]).map_err(Error::io)?;
    String::from_utf8(buf).map_err(|e| {
        Error::from(InnerError::Utf8Error {
            source: e.utf8_error(),
            buf: e.into_bytes(),
        })
    })
}

fn decode_text_of<R: Read>(
    decoder: &mut ciborium_ll::Decoder<R>,
    ty: TypeInfo,
    text: &str,
) -> Result<(), Error<R::Error>> {
    match decoder.pull().map_err(InnerError::Cbor)? {
        Header::Text(Some(l)) if l == text.len() => (),
        Header::Text(Some(l)) => {
            return Err(Error::from(InnerError::UnknownField {
                ty,
                field: read_string(Vec::with_capacity(l), l, decoder)?,
            }))
        }
        h => return Err(Error::prim_type_error::<&str>(h)),
    }

    const CHUNK_SIZE: usize = 16;
    for (idx, v) in text.as_bytes().chunks(CHUNK_SIZE).enumerate() {
        let mut chunk_buf = [0; CHUNK_SIZE];
        let buf = &mut chunk_buf[0..v.len()];
        decoder.read_exact(buf).map_err(Error::io)?;
        if buf != v {
            let mut field = Vec::with_capacity(text.len());
            field.extend_from_slice(&text.as_bytes()[0..idx * CHUNK_SIZE]); // matched prefix
            field.extend_from_slice(buf);
            return Err(Error::from(InnerError::UnknownField {
                ty,
                field: read_string(field, text.len(), decoder)?,
            }));
        }
    }
    Ok(())
}

pub enum Enum<'t, 'd, R: Read> {
    Unit(&'t str),
    Tuple(&'t str, usize, TupleVariantDecoder<'d, R>),
    Struct(&'t str, usize, StructVariantDecoder<'d, R>),
}

pub struct Decoder<'a, R: Read>(pub(in crate::codec::cbor) &'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> Decoder<'a, R> {
    fn decode_unsigned<T: TryFrom<u64>>(self) -> Result<T, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            h @ Header::Positive(v) => match T::try_from(v) {
                Ok(r) => Ok(r),
                Err(_) => Err(Error::prim_type_error::<T>(h)),
            },
            h => Err(Error::prim_type_error::<T>(h)),
        }
    }
    fn decode_signed<T: TryFrom<i64>>(self) -> Result<T, Error<R::Error>> {
        let (h, v) = match self.0.pull().map_err(InnerError::Cbor)? {
            h @ Header::Positive(v) if v <= (i64::MAX as u64) => (h, v as i64),
            h @ Header::Negative(v) if v <= ((i64::MIN as u64) ^ !0) => (h, (v ^ !0) as i64),
            h => return Err(Error::prim_type_error::<T>(h)),
        };
        match T::try_from(v) {
            Ok(r) => Ok(r),
            Err(_) => Err(Error::prim_type_error::<T>(h)),
        }
    }

    pub fn decode_tuple_struct_len(
        self,
        ty: TypeInfo,
        size: usize,
    ) -> Result<TupleStructDecoder<'a, R>, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Array(Some(v)) => {
                if v == size {
                    Ok(TupleStructDecoder(self.0))
                } else {
                    Err(Error::from(InnerError::SizeMismatch {
                        ty,
                        actual: v,
                        expect: size,
                    }))
                }
            }
            h => Err(Error::from(InnerError::TypeError { ty, actual: h })),
        }
    }
    pub fn decode_struct_len(
        self,
        ty: TypeInfo,
        size: usize,
    ) -> Result<StructDecoder<'a, R>, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Map(Some(v)) => {
                if v == size {
                    Ok(StructDecoder {
                        ty,
                        decoder: self.0,
                    })
                } else {
                    Err(Error::from(InnerError::SizeMismatch {
                        ty,
                        actual: v,
                        expect: size,
                    }))
                }
            }
            h => Err(Error::from(InnerError::TypeError { ty, actual: h })),
        }
    }

    pub fn decode_list(self, ty: TypeInfo) -> Result<(usize, ListDecoder<'a, R>), Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Array(Some(l)) => Ok((l, ListDecoder(self.0))),
            h => Err(Error::from(InnerError::TypeError { ty, actual: h })),
        }
    }
    pub fn decode_list_len(
        self,
        ty: TypeInfo,
        len: usize,
    ) -> Result<ListDecoder<'a, R>, Error<R::Error>> {
        let (actual_len, ret) = self.decode_list(ty)?;
        if actual_len == len {
            Ok(ret)
        } else {
            Err(Error::from(InnerError::SizeMismatch {
                ty,
                actual: actual_len,
                expect: len,
            }))
        }
    }
    pub fn decode_map(self, ty: TypeInfo) -> Result<(usize, MapDecoder<'a, R>), Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Map(Some(l)) => Ok((l, MapDecoder(self.0))),
            h => Err(Error::from(InnerError::TypeError { ty, actual: h })),
        }
    }
    pub fn decode_map_len(
        self,
        ty: TypeInfo,
        len: usize,
    ) -> Result<MapDecoder<'a, R>, Error<R::Error>> {
        let (actual_len, ret) = self.decode_map(ty)?;
        if actual_len == len {
            Ok(ret)
        } else {
            Err(Error::from(InnerError::SizeMismatch {
                ty,
                actual: actual_len,
                expect: len,
            }))
        }
    }

    pub fn decode_enum<'t>(
        self,
        ty: TypeInfo,
        buf: &'t mut [u8],
    ) -> Result<Enum<'t, 'a, R>, Error<R::Error>> {
        fn read_name<'t, R: Read>(
            decoder: &mut ciborium_ll::Decoder<R>,
            ty: TypeInfo,
            len: usize,
            buf: &'t mut [u8],
        ) -> Result<&'t str, Error<R::Error>> {
            if buf.len() < len {
                return Err(Error::from(InnerError::UnknownVariant {
                    ty,
                    variant: read_string(Vec::with_capacity(len), len, decoder)?,
                }));
            }
            let buf = &mut buf[0..len];
            decoder.read_exact(buf).map_err(Error::io)?;
            std::str::from_utf8(buf).map_err(|e| {
                Error::from(InnerError::Utf8Error {
                    buf: buf.to_owned(),
                    source: e,
                })
            })
        }

        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Text(Some(t)) => Ok(Enum::Unit(read_name(self.0, ty, t, buf)?)),
            Header::Tag(super::ENUM_TAG) => {
                match self.0.pull().map_err(InnerError::Cbor)? {
                    Header::Array(Some(2)) => (),
                    h => {
                        return Err(Error::from(InnerError::InvalidEnum {
                            ty,
                            expected: "array of variant name and body",
                            header: h,
                        }))
                    }
                }
                let name = match self.0.pull().map_err(InnerError::Cbor)? {
                    Header::Text(Some(t)) => read_name(&mut *self.0, ty, t, buf)?,
                    h => {
                        return Err(Error::from(InnerError::InvalidEnum {
                            ty,
                            expected: "variant name",
                            header: h,
                        }))
                    }
                };
                match self.0.pull().map_err(InnerError::Cbor)? {
                    Header::Array(Some(l)) => Ok(Enum::Tuple(name, l, TupleVariantDecoder(self.0))),
                    Header::Map(Some(l)) => Ok(Enum::Struct(
                        name,
                        l,
                        StructVariantDecoder {
                            ty,
                            decoder: self.0,
                        },
                    )),
                    h => Err(Error::from(InnerError::InvalidEnum {
                        ty,
                        expected: "array or map body",
                        header: h,
                    })),
                }
            }
            h => Err(Error::from(InnerError::TypeError { ty, actual: h })),
        }
    }
}

pub struct TupleStructDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> TupleStructDecoder<'a, R> {
    pub fn next_field<F: FromCbor<R>>(&mut self) -> Result<F, Error<R::Error>> {
        F::decode(Decoder(&mut *self.0))
    }
}
pub struct StructDecoder<'a, R: Read> {
    ty: TypeInfo,
    decoder: &'a mut ciborium_ll::Decoder<R>,
}
impl<'a, R: Read> StructDecoder<'a, R> {
    pub fn next_field_of<F: FromCbor<R>>(
        &mut self,
        field: &'static str,
    ) -> Result<F, Error<R::Error>> {
        decode_text_of(self.decoder, self.ty, field)?;
        F::decode(Decoder(&mut *self.decoder))
    }
}

pub struct TupleVariantDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> TupleVariantDecoder<'a, R> {
    pub fn next_field<F: FromCbor<R>>(&mut self) -> Result<F, Error<R::Error>> {
        F::decode(Decoder(&mut *self.0))
    }
}

pub struct StructVariantDecoder<'a, R: Read> {
    ty: TypeInfo,
    decoder: &'a mut ciborium_ll::Decoder<R>,
}
impl<'a, R: Read> StructVariantDecoder<'a, R> {
    pub fn next_field_of<F: FromCbor<R>>(
        &mut self,
        field: &'static str,
    ) -> Result<F, Error<R::Error>> {
        decode_text_of(self.decoder, self.ty, field)?;
        F::decode(Decoder(&mut *self.decoder))
    }
}

pub struct ListDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> ListDecoder<'a, R> {
    pub fn next_element<F: FromCbor<R>>(&mut self) -> Result<F, Error<R::Error>> {
        F::decode(Decoder(&mut *self.0))
    }
}

pub struct MapDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> MapDecoder<'a, R> {
    pub fn decode_entry<K: FromCbor<R>, V: FromCbor<R>>(
        &mut self,
    ) -> Result<(K, V), Error<R::Error>> {
        let key = K::decode(Decoder(&mut *self.0))?;
        Ok((key, V::decode(Decoder(&mut *self.0))?))
    }
}

pub trait FromCbor<R: Read>: Sized {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<R::Error>>;
}

pub trait DecodeSlice: for<'a> FromCbor<Reader<'a>> {}
impl<T> DecodeSlice for T where T: for<'a> FromCbor<Reader<'a>> {}

impl<R: Read> FromCbor<R> for bool {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<R::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Simple(simple::TRUE) => Ok(true),
            Header::Simple(simple::FALSE) => Ok(false),
            h => Err(Error::prim_type_error::<bool>(h)),
        }
    }
}

macro_rules! unsigned_impl {
    ($t:ty) => {
        impl<R: Read> FromCbor<R> for $t {
            fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
                decoder.decode_unsigned()
            }
        }
    };
}
unsigned_impl!(u8);
unsigned_impl!(u16);
unsigned_impl!(u32);
unsigned_impl!(u64);

macro_rules! signed_impl {
    ($t:ty) => {
        impl<R: Read> FromCbor<R> for $t {
            fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
                decoder.decode_signed()
            }
        }
    };
}
signed_impl!(i8);
signed_impl!(i16);
signed_impl!(i32);
signed_impl!(i64);

impl<R: Read> FromCbor<R> for () {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Simple(simple::NULL) => Ok(()),
            h => Err(Error::prim_type_error::<()>(h)),
        }
    }
}
impl<const N: usize, R: Read, T: FromCbor<R>> FromCbor<R> for [T; N] {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let mut dec = decoder.decode_list_len(type_name::<Self>(), N)?;
        let mut ret = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        for i in 0..N {
            ret[i].write(dec.next_element()?);
        }
        unsafe {
            let r = ptr::read(&ret as *const [MaybeUninit<T>; N] as *const [T; N]);
            std::mem::forget(ret);
            Ok(r)
        }
    }
}

impl<R: Read, T: FromCbor<R>> FromCbor<R> for Option<T> {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Simple(simple::NULL) => Ok(None),
            h => {
                decoder.0.push(h);
                T::decode(decoder).map(Some)
            }
        }
    }
}

impl<R: Read> FromCbor<R> for String {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let len = match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Text(Some(v)) => v,
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    actual: h,
                }))
            }
        };
        read_string(Vec::with_capacity(len), len, decoder.0)
    }
}
impl<R: Read> FromCbor<R> for ByteBuf {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let len = match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Bytes(Some(v)) => v,
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    actual: h,
                }))
            }
        };
        let mut ret = Vec::with_capacity(len);
        ret.resize(len, 0);
        decoder.0.read_exact(&mut ret).map_err(Error::io)?;
        Ok(ByteBuf(ret))
    }
}

impl<R: Read, T: FromCbor<R>> FromCbor<R> for Vec<T> {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let (len, mut decoder) = decoder.decode_list(type_name::<Self>())?;
        let mut ret = Vec::with_capacity(len);
        for _ in 0..len {
            ret.push(decoder.next_element()?);
        }
        Ok(ret)
    }
}

impl<R: Read, T: FromCbor<R> + Ord> FromCbor<R> for BTreeSet<T> {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let (len, mut decoder) = decoder.decode_list(type_name::<Self>())?;
        let mut ret = BTreeSet::new();
        for _ in 0..len {
            ret.insert(decoder.next_element()?);
        }
        Ok(ret)
    }
}
impl<R: Read, K: FromCbor<R> + Ord, V: FromCbor<R>> FromCbor<R> for BTreeMap<K, V> {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        let (len, mut decoder) = decoder.decode_map(type_name::<Self>())?;
        let mut ret = BTreeMap::new();
        for _ in 0..len {
            let (k, v) = decoder.decode_entry()?;
            ret.insert(k, v);
        }
        Ok(ret)
    }
}

impl<R: Read> FromCbor<R> for uuid::Uuid {
    fn decode<'a>(decoder: Decoder<'a, R>) -> Result<Self, Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Tag(super::UUID_TAG) => (),
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    actual: h,
                }))
            }
        }
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Bytes(Some(16)) => {
                let mut buf = [0; 16];
                decoder.0.read_exact(&mut buf).map_err(Error::io)?;
                Ok(uuid::Uuid::from_bytes(buf))
            }
            h => Err(Error::from(InnerError::TypeError {
                ty: type_name::<Self>(),
                actual: h,
            })),
        }
    }
}
