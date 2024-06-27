use std::fmt::Debug;

use webar_core::Timestamp;
use webar_data::ser::Serialize;

fn test_serde<D: Debug + Eq + Serialize + serde::de::DeserializeOwned>(data: D, bin: &[u8]) {
    assert_eq!(webar_data::cbor::to_vec(&data), bin, "serialize");
    assert_eq!(
        ciborium::from_reader::<D, _>(bin).unwrap(),
        data,
        "deserialize"
    )
}

mod digest {
    use webar_core::digest::{Digest, Sha256};

    use crate::test_serde;

    const SHA256_EMPTY: Sha256 = Sha256(hex_literal::hex!(
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    ));

    #[test]
    fn sha256_empty() {
        test_serde(SHA256_EMPTY, include_bytes!("./cbor/sha256_empty.bin"))
    }

    #[test]
    fn digest_sha256_empty() {
        test_serde(
            Digest::Sha256(SHA256_EMPTY),
            include_bytes!("./cbor/digest_sha256_empty.bin"),
        )
    }
}

#[test]
fn timestamp_0() {
    test_serde(
        Timestamp { secs: 0, nanos: 0 },
        include_bytes!("./cbor/timestamp_0.bin"),
    )
}

#[test]
fn timestamp_1980() {
    test_serde(
        Timestamp {
            secs: 315664496,
            nanos: 123400000,
        },
        include_bytes!("./cbor/timestamp_1980.bin"),
    )
}
