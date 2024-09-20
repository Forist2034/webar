use hex_literal::hex;

use webar_core::digest::{Digest, Sha256};
use webar_stackexchange_core::rest_api::filter::{FilterId, FilterSpec, TypeMap};

macro_rules! filter_sha256 {
    ($name:literal, $id:literal) => {
        FilterSpec {
            name: $name,
            id: FilterId::new(Digest::Sha256(Sha256(hex!($id)))),
        }
    };
}

pub const HS_FILTER_INFO: TypeMap<FilterSpec<&str>> = TypeMap {
    answer: filter_sha256!(
        ".BcrXh4RU(M_r5Qrar94ww5zJfMO32fPy",
        "0f37fe30b53c15e4aff78617d2b43853671acff65b2f4272152f5ebdad3ac49f"
    ),
    badge: filter_sha256!(
        "3pH)E3HUeKQ7hx0WYJWum",
        "a1c39f8374bb8177b93532963f8254542a3dbe9e32b0b126182269324486c8b6"
    ),
    comment: filter_sha256!(
        ")OgkWFXtkRBL.fntaKvbDSX(siri",
        "2ea0f50afe9b98402df62c01a76f87c4e45555b29fbecec3cf96646ad75aec63"
    ),
    collective: filter_sha256!(
        "aIKY(zS3d5vv.dREz*",
        "c5867924d68836e6f4fbccf7b5023d8661f3c88542d8c7659ce4a484411b1dd5"
    ),
    info: filter_sha256!(
        "S9)8yVzYJPOeK*_4YAeTUD",
        "48e85b468392e8b73caf37fbc7924671f155acc7cd156caed585a3b8287c2072"
    ),
    question: filter_sha256!(
        "Csa39LsSH3q3d26SekiZhUTEepAsWwXYs(tUH*DF.J-q)D4l0fbs-z*",
        "2ab30cca11aed2e7b1d86e78ef37d43c46e8fd0b180f34ced7fb0bfd0b74d35d"
    ),
    revision: filter_sha256!(
        "S0LW7y9G39VsSNOC*FQvB9",
        "b75984a178edc4d93015796c119977be76d22169cb62fabf756546cc15076189"
    ),
    tag: filter_sha256!(
        ")pmU07Nl*8faPpF39V*n6)ID",
        "501effc565f1625ba162091159d277a4219900d71bb5e23edf19dbddb3afb22a"
    ),
    tag_synonym: filter_sha256!(
        "a_LjP(LbK3SaJfWuW5",
        "f3f506ac1a5faa2b517a66d8e71ddfbe9ac6263284b5f3a610446b24949566f8"
    ),
    tag_wiki: filter_sha256!(
        "SIUtpsPZ-9l)fUK)qR2IvD",
        "2ceb4965724c7118f0f03537c57498b11950de7e1641b0219613aac02d150c6a"
    ),
    user: filter_sha256!(
        ")4lk-t8lU0Au6PWx0o1erLmkS(mCImce",
        "48e8751a0a138a50f3404f40d7abe59e6933bbc87e95ba903ffca0677b101409"
    ),
};
