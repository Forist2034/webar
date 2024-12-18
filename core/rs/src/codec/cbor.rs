/*! Deterministic serialization and deserialization for cbor

Encoder and decoder api is experimental and should use proc-macro instead.

May split into separate crate and reexport there when encoder and decoder api is stabilized.
Now we put these in core so that types that require manual encode and decode impl
like [crate::Timestamp] can be handled. For types in other crates that require
manual encode and decode, define them here and add `#[doc(hidden)]` and
reexport in that crate as a temporary measure.
 */
use std::convert::Infallible;

#[doc(inline)]
pub use self::internal::{decoding::DecodeSlice, encoding::ToCbor};
/// Shorthand for deriving both [DecodeSlice] and [ToCbor]
pub use webar_core_derive::CborCodec;
#[doc(inline)]
pub use webar_core_derive::{FromCbor, ToCbor};

/// Internal api for cbor encoding and decoding
pub mod internal {
    const ENUM_TAG: u64 = 27;
    const UUID_TAG: u64 = 37;

    #[doc(hidden)]
    pub extern crate std;

    pub type TypeInfo = &'static str;

    pub mod decoding;
    pub mod encoding;
}

pub fn to_vec<T: ToCbor + ?Sized>(value: &T) -> Vec<u8> {
    struct VecWriter(Vec<u8>);
    impl ciborium_io::Write for &mut VecWriter {
        type Error = Infallible;
        fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
            self.0.extend_from_slice(data);
            Ok(())
        }
        fn flush(&mut self) -> Result<(), Self::Error> {
            Ok(())
        }
    }
    let mut writer = VecWriter(Vec::new());
    let mut encoder = ciborium_ll::Encoder::from(&mut writer);
    match value.encode(internal::encoding::Encoder(&mut encoder)) {
        Ok(()) => (),
        Err(e) => match e.0 {},
    }
    writer.0
}

pub type DecodeSliceError = internal::decoding::Error<internal::decoding::ReadError>;

pub fn from_slice<T: DecodeSlice>(slice: &[u8]) -> Result<T, DecodeSliceError> {
    T::decode(internal::decoding::Decoder(
        &mut ciborium_ll::Decoder::from(internal::decoding::Reader::new(slice)),
    ))
}
