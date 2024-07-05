//! # json serializer
//! based on [serde_json]

use std::io;

use serde::Deserialize;
use serde_json::ser::{CharEscape, CompactFormatter, Formatter};

#[derive(Clone, Copy)]
enum Escape {
    /// no escape
    __,
    BB,
    TT,
    NN,
    FF,
    RR,
    QU,
    BS,
    UU,
}
impl Escape {
    #[inline]
    fn to_char_escape(self, byte: u8) -> Option<CharEscape> {
        match self {
            Self::__ => None,
            Self::BB => Some(CharEscape::Backspace),
            Self::TT => Some(CharEscape::Tab),
            Self::NN => Some(CharEscape::LineFeed),
            Self::FF => Some(CharEscape::FormFeed),
            Self::RR => Some(CharEscape::CarriageReturn),
            Self::QU => Some(CharEscape::Quote),
            Self::BS => Some(CharEscape::ReverseSolidus),
            Self::UU => Some(CharEscape::AsciiControl(byte)),
        }
    }
}

/// char escape table, from [serde_json]
static ESCAPE: [Escape; 256] = {
    use Escape::*;
    [
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
        UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
        __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
    ]
};

fn format_escaped_str_contents<W, F>(
    writer: &mut W,
    formatter: &mut F,
    value: &str,
) -> io::Result<()>
where
    W: ?Sized + io::Write,
    F: ?Sized + Formatter,
{
    let bytes = value.as_bytes();

    let mut start = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        if let Some(ce) = ESCAPE[byte as usize].to_char_escape(byte) {
            if start < i {
                formatter.write_string_fragment(writer, &value[start..i])?;
            }

            formatter.write_char_escape(writer, ce)?;

            start = i + 1;
        }
    }

    if start == bytes.len() {
        return Ok(());
    }

    formatter.write_string_fragment(writer, &value[start..])
}

fn format_escaped_str<W, F>(writer: &mut W, formatter: &mut F, value: &str) -> io::Result<()>
where
    W: ?Sized + io::Write,
    F: ?Sized + Formatter,
{
    formatter.begin_string(writer)?;
    format_escaped_str_contents(writer, formatter, value)?;
    formatter.end_string(writer)
}

#[derive(Debug)]
pub enum Error {
    NonStringKey,
    Bytes,
    Io(io::Error),
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NonStringKey => f.write_str("object key must be string"),
            Self::Bytes => f.write_str("bytes is not supported"),
            Self::Io(e) => write!(f, "io error: {e}"),
        }
    }
}
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let Self::Io(e) = self {
            Some(e)
        } else {
            None
        }
    }
}
impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

pub struct Serializer<W> {
    writer: W,
    formatter: CompactFormatter,
}
impl<W> Serializer<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            formatter: CompactFormatter,
        }
    }
}
impl<W: io::Write> Serializer<W> {
    fn begin_variant(&mut self, variant: &'static str) -> Result<(), Error> {
        self.formatter.begin_object(&mut self.writer)?;

        self.formatter.begin_object_key(&mut self.writer, true)?;
        format_escaped_str(&mut self.writer, &mut self.formatter, variant)?;
        self.formatter.end_object_key(&mut self.writer)?;

        self.formatter
            .begin_object_value(&mut self.writer)
            .map_err(Error::Io)
    }
    fn end_variant(&mut self) -> Result<(), Error> {
        self.formatter.end_object_value(&mut self.writer)?;
        self.formatter
            .end_object(&mut self.writer)
            .map_err(Error::Io)
    }
}

struct Inner<'a, W> {
    serializer: &'a mut Serializer<W>,
    first: bool,
}

pub struct ArraySerializer<'a, W>(Inner<'a, W>);
impl<'a, W: io::Write> ArraySerializer<'a, W> {
    fn serialize_elem<D: ?Sized + crate::ser::Serialize>(
        &mut self,
        value: &D,
    ) -> Result<(), Error> {
        self.0
            .serializer
            .formatter
            .begin_array_value(&mut self.0.serializer.writer, self.0.first)?;
        value.serialize(&mut *self.0.serializer)?;
        self.0
            .serializer
            .formatter
            .end_array_value(&mut self.0.serializer.writer)?;
        self.0.first = false;
        Ok(())
    }
    fn end_array(&mut self) -> Result<(), Error> {
        self.0
            .serializer
            .formatter
            .end_array(&mut self.0.serializer.writer)
            .map_err(Error::Io)
    }
}
impl<'a, W: io::Write> crate::ser::SerializeList for ArraySerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.serialize_elem(value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_array()
    }
}
impl<'a, W: io::Write> crate::ser::SerializeTupleStruct for ArraySerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.serialize_elem(value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_array()
    }
}
impl<'a, W: io::Write> crate::ser::SerializeTupleVariant for ArraySerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.serialize_elem(value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_array()?;
        self.0.serializer.end_variant()
    }
}

enum Impossible {}
impl crate::ser::SerializeList for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_element<T>(&mut self, _: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}
impl crate::ser::SerializeMap for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_entry<K, V>(&mut self, _: &K, _: &V) -> Result<(), Self::Error>
    where
        K: ?Sized + crate::ser::Serialize,
        V: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}
impl crate::ser::SerializeStruct for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, _: &'static str, _: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}
impl crate::ser::SerializeStructVariant for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, _: &'static str, _: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}
impl crate::ser::SerializeTupleStruct for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, _: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}
impl crate::ser::SerializeTupleVariant for Impossible {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, _: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        match *self {}
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        match self {}
    }
}

struct KeySerializer<'a, W>(&'a mut Serializer<W>);
impl<'a, W: io::Write> KeySerializer<'a, W> {
    fn write_key<V>(
        self,
        f: impl Fn(&mut CompactFormatter, &mut W, V) -> io::Result<()>,
        val: V,
    ) -> Result<(), Error> {
        self.0.formatter.begin_string(&mut self.0.writer)?;
        f(&mut self.0.formatter, &mut self.0.writer, val)?;
        self.0
            .formatter
            .end_string(&mut self.0.writer)
            .map_err(Error::Io)
    }
}
impl<'a, W: io::Write> crate::ser::Serializer for KeySerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    type SerializeList = Impossible;
    type SerializeMap = Impossible;
    type SerializeStruct = Impossible;
    type SerializeStructVariant = Impossible;
    type SerializeTupleStruct = Impossible;
    type SerializeTupleVariant = Impossible;

    const HUMAN_READABLE: bool = true;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_bool, v)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_i8, v)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_i16, v)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_i32, v)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_i64, v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_u8, v)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_u16, v)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_u32, v)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.write_key(CompactFormatter::write_u64, v)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        format_escaped_str(&mut self.0.writer, &mut self.0.formatter, v).map_err(Error::Io)
    }
    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::Bytes)
    }

    fn serialize_some<T>(self, _: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        Err(Error::NonStringKey)
    }
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::NonStringKey)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::NonStringKey)
    }

    fn serialize_list(self, _: usize) -> Result<Self::SerializeList, Self::Error> {
        Err(Error::NonStringKey)
    }
    fn serialize_map(self, _: usize) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::NonStringKey)
    }
    fn serialize_newtype_variant<T>(
        self,
        // name: &'static str,
        // variant_index: u32,
        _: &'static str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        Err(Error::NonStringKey)
    }
    fn serialize_unit_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        format_escaped_str(&mut self.0.writer, &mut self.0.formatter, variant).map_err(Error::Io)
    }
    fn serialize_struct_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::NonStringKey)
    }
    fn serialize_tuple_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::NonStringKey)
    }
    fn serialize_tuple_struct(
        self,
        // name: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::NonStringKey)
    }
    fn serialize_struct(
        self,
        // name: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::NonStringKey)
    }
}

pub struct ObjectSerializer<'a, W>(Inner<'a, W>);
impl<'a, W: io::Write> ObjectSerializer<'a, W> {
    fn serialize_ent<K, V>(&mut self, key: &K, value: &V) -> Result<(), Error>
    where
        K: ?Sized + crate::ser::Serialize,
        V: ?Sized + crate::ser::Serialize,
    {
        self.0
            .serializer
            .formatter
            .begin_object_key(&mut self.0.serializer.writer, self.0.first)?;
        key.serialize(KeySerializer(&mut *self.0.serializer))?;
        self.0
            .serializer
            .formatter
            .end_object_key(&mut self.0.serializer.writer)?;

        self.0
            .serializer
            .formatter
            .begin_object_value(&mut self.0.serializer.writer)?;
        value.serialize(&mut *self.0.serializer)?;
        self.0
            .serializer
            .formatter
            .end_object_value(&mut self.0.serializer.writer)?;

        self.0.first = false;
        Ok(())
    }
    fn end_object(&mut self) -> Result<(), Error> {
        self.0
            .serializer
            .formatter
            .end_object(&mut self.0.serializer.writer)
            .map_err(Error::Io)
    }
}
impl<'a, W: io::Write> crate::ser::SerializeMap for ObjectSerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Self::Error>
    where
        K: ?Sized + crate::ser::Serialize,
        V: ?Sized + crate::ser::Serialize,
    {
        self.serialize_ent(key, value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_object()
    }
}
impl<'a, W: io::Write> crate::ser::SerializeStructVariant for ObjectSerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.serialize_ent(key, value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_object()?;
        self.0.serializer.end_variant()
    }
}
impl<'a, W: io::Write> crate::ser::SerializeStruct for ObjectSerializer<'a, W> {
    type Ok = ();
    type Error = Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.serialize_ent(key, value)
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.end_object()
    }
}

impl<'a, W: io::Write> crate::ser::Serializer for &'a mut Serializer<W> {
    type Ok = ();
    type Error = Error;
    type SerializeList = ArraySerializer<'a, W>;
    type SerializeMap = ObjectSerializer<'a, W>;
    type SerializeStruct = ObjectSerializer<'a, W>;
    type SerializeStructVariant = ObjectSerializer<'a, W>;
    type SerializeTupleStruct = ArraySerializer<'a, W>;
    type SerializeTupleVariant = ArraySerializer<'a, W>;

    const HUMAN_READABLE: bool = true;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_bool(&mut self.writer, v)
            .map_err(Error::Io)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_i8(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_i16(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_i32(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_i64(&mut self.writer, v)
            .map_err(Error::Io)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_u8(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_u16(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_u32(&mut self.writer, v)
            .map_err(Error::Io)
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_u64(&mut self.writer, v)
            .map_err(Error::Io)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        format_escaped_str(&mut self.writer, &mut self.formatter, v).map_err(Error::Io)
    }
    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::Bytes)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        value.serialize(self)
    }
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_null(&mut self.writer)
            .map_err(Error::Io)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.formatter
            .write_null(&mut self.writer)
            .map_err(Error::Io)
    }

    fn serialize_unit_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        format_escaped_str(&mut self.writer, &mut self.formatter, variant).map_err(Error::Io)
    }
    fn serialize_newtype_variant<T>(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        self.begin_variant(variant)?;
        value.serialize(&mut *self)?;
        self.end_variant()
    }
    fn serialize_tuple_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.begin_variant(variant)?;
        self.formatter.begin_array(&mut self.writer)?;
        Ok(ArraySerializer(Inner {
            serializer: self,
            first: true,
        }))
    }
    fn serialize_struct_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.begin_variant(variant)?;
        self.formatter.begin_object(&mut self.writer)?;
        Ok(ObjectSerializer(Inner {
            serializer: self,
            first: true,
        }))
    }
    fn serialize_tuple_struct(
        self,
        // name: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.formatter.begin_array(&mut self.writer)?;
        Ok(ArraySerializer(Inner {
            serializer: self,
            first: true,
        }))
    }
    fn serialize_struct(
        self,
        // name: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.formatter.begin_object(&mut self.writer)?;
        Ok(ObjectSerializer(Inner {
            serializer: self,
            first: true,
        }))
    }

    fn serialize_list(self, _: usize) -> Result<Self::SerializeList, Self::Error> {
        self.formatter.begin_array(&mut self.writer)?;
        Ok(ArraySerializer(Inner {
            serializer: self,
            first: true,
        }))
    }
    fn serialize_map(self, _: usize) -> Result<Self::SerializeMap, Self::Error> {
        self.formatter.begin_object(&mut self.writer)?;
        Ok(ObjectSerializer(Inner {
            serializer: self,
            first: true,
        }))
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(transparent)]
pub struct RawValue(pub serde_json::Value);
impl RawValue {
    fn into_formatter<W: io::Write>(
        self,
        formatter: &mut CompactFormatter,
        writer: &mut W,
    ) -> io::Result<()> {
        use serde_json::Value;
        match self.0 {
            Value::Null => formatter.write_null(writer),
            Value::Bool(b) => formatter.write_bool(writer, b),
            Value::Number(n) => formatter.write_number_str(writer, n.as_str()),
            Value::String(s) => format_escaped_str(writer, formatter, s.as_str()),
            Value::Array(a) => {
                formatter.begin_array(writer)?;
                let mut first = true;
                for v in a {
                    formatter.begin_array_value(writer, first)?;
                    RawValue(v).into_formatter(formatter, writer)?;
                    formatter.end_array_value(writer)?;
                    first = false;
                }
                formatter.end_array(writer)
            }
            Value::Object(o) => {
                let mut keys: Vec<_> = o.into_iter().collect();
                keys.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
                let mut first = true;
                formatter.begin_object(writer)?;
                for (k, v) in keys {
                    formatter.begin_object_key(writer, first)?;
                    format_escaped_str(writer, formatter, k.as_str())?;
                    formatter.end_object_key(writer)?;

                    formatter.begin_object_value(writer)?;
                    RawValue(v).into_formatter(formatter, writer)?;
                    formatter.end_object_value(writer)?;
                    first = false;
                }
                formatter.end_object(writer)
            }
        }
    }
    pub fn into_vec(self) -> Vec<u8> {
        let mut ret = Vec::new();
        self.into_formatter(&mut CompactFormatter, &mut ret)
            .unwrap();
        ret
    }
    pub fn into_string(self) -> String {
        unsafe { String::from_utf8_unchecked(self.into_vec()) }
    }
}

pub fn to_vec<D: crate::ser::Serialize>(value: &D) -> Result<Vec<u8>, Error> {
    let mut ret = Vec::new();
    value.serialize(&mut Serializer::new(&mut ret))?;
    Ok(ret)
}
pub fn to_string<D: crate::ser::Serialize>(value: &D) -> Result<String, Error> {
    to_vec(value).map(|v| unsafe { String::from_utf8_unchecked(v) })
}
