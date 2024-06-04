use std::convert::Infallible;

use ciborium_io::Write;
use ciborium_ll::{simple, Header};

use crate::ser::{
    Serialize, SerializeList, SerializeMap, SerializeStruct, SerializeStructVariant,
    SerializeTupleStruct, SerializeTupleVariant, Serializer,
};

pub struct Encoder<W: Write>(ciborium_ll::Encoder<W>);
impl<W: Write> Encoder<W> {
    fn push_text(&mut self, v: &str) -> Result<(), W::Error> {
        self.0.push(Header::Text(Some(v.len())))?;
        self.0.write_all(v.as_bytes())
    }
    fn push_bytes(&mut self, v: &[u8]) -> Result<(), W::Error> {
        self.0.push(Header::Bytes(Some(v.len())))?;
        self.0.write_all(v)
    }
}

pub struct MapEncoder<'a, W: Write>(&'a mut Encoder<W>);
impl<'a, W: Write> MapEncoder<'a, W> {
    fn add_entry<K: ?Sized + Serialize, V: ?Sized + Serialize>(
        &mut self,
        k: &K,
        v: &V,
    ) -> Result<(), W::Error> {
        k.serialize(&mut *self.0)?;
        v.serialize(&mut *self.0)
    }
}

pub struct ListEncoder<'a, W: Write>(&'a mut Encoder<W>);

impl<'a, W: Write> SerializeList for ListEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        value.serialize(&mut *self.0)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
impl<'a, W: Write> SerializeTupleStruct for ListEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        value.serialize(&mut *self.0)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
impl<'a, W: Write> SerializeTupleVariant for ListEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::ser::Serialize,
    {
        value.serialize(&mut *self.0)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a, W: Write> SerializeMap for MapEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;
    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Self::Error>
    where
        K: ?Sized + crate::ser::Serialize,
        V: ?Sized + crate::ser::Serialize,
    {
        self.add_entry(key, value)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
impl<'a, W: Write> SerializeStruct for MapEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.add_entry(key, value)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
impl<'a, W: Write> SerializeStructVariant for MapEncoder<'a, W> {
    type Ok = ();
    type Error = W::Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.add_entry(key, value)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<'a, W: Write> Serializer for &'a mut Encoder<W> {
    type Ok = ();
    type Error = W::Error;

    type SerializeList = ListEncoder<'a, W>;
    type SerializeTupleStruct = ListEncoder<'a, W>;
    type SerializeTupleVariant = ListEncoder<'a, W>;
    type SerializeMap = MapEncoder<'a, W>;
    type SerializeStruct = MapEncoder<'a, W>;
    type SerializeStructVariant = MapEncoder<'a, W>;

    const HUMAN_READABLE: bool = false;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Simple(match v {
            true => simple::TRUE,
            false => simple::FALSE,
        }))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.serialize_i64(v as i64)
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.0.push(match v.is_negative() {
            false => Header::Positive(v as u64),
            true => Header::Negative((v as u64) ^ !0),
        })
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Positive(v as u64))
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Positive(v as u64))
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Positive(v as u64))
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Positive(v))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.push_text(v)
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.push_bytes(v)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.0.push(Header::Simple(simple::NULL))
    }
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.push_text(variant)
    }
    fn serialize_newtype_variant<T>(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.0.push(Header::Map(Some(1)))?;
        self.push_text(variant)?;
        value.serialize(self)
    }
    fn serialize_tuple_struct(
        self,
        // name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.0.push(Header::Array(Some(len)))?;
        Ok(ListEncoder(self))
    }
    fn serialize_tuple_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.0.push(Header::Map(Some(1)))?;
        self.push_text(variant)?;
        self.0.push(Header::Array(Some(len)))?;
        Ok(ListEncoder(self))
    }

    fn serialize_struct(
        self,
        // name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.0.push(Header::Map(Some(len)))?;
        Ok(MapEncoder(self))
    }
    fn serialize_struct_variant(
        self,
        // name: &'static str,
        // variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.0.push(Header::Map(Some(1)))?;
        self.push_text(variant)?;
        self.0.push(Header::Map(Some(len)))?;
        Ok(MapEncoder(self))
    }

    fn serialize_map(self, len: usize) -> Result<Self::SerializeMap, Self::Error> {
        self.0.push(Header::Map(Some(len)))?;
        Ok(MapEncoder(self))
    }
    fn serialize_list(self, len: usize) -> Result<Self::SerializeList, Self::Error> {
        self.0.push(Header::Array(Some(len)))?;
        Ok(ListEncoder(self))
    }
}

pub fn to_vec<S: Serialize>(data: &S) -> Vec<u8> {
    struct Buf(Vec<u8>);
    impl Write for &mut Buf {
        type Error = Infallible;
        fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
            self.0.extend(data);
            Ok(())
        }
        fn flush(&mut self) -> Result<(), Self::Error> {
            Ok(())
        }
    }
    let mut ret = Buf(Vec::new());
    data.serialize(&mut Encoder(ciborium_ll::Encoder::from(&mut ret)))
        .unwrap();
    ret.0
}
