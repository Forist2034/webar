use hex_literal::hex;

use webar_core::digest::{Digest, Sha256};
use webar_stackexchange_core::rest_api::filter::{FilterId, FilterSpec, TypeMap};

pub const HS_FILTER_INFO: TypeMap<FilterSpec<&str>> = TypeMap {
    answer: FilterSpec {
        name: ".BcrXh4RU(M_r5Qrar94ww5zJfMO32fPy",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "0f37fe30b53c15e4aff78617d2b43853671acff65b2f4272152f5ebdad3ac49f"
        )))),
    },
    badge: FilterSpec {
        name: "3pH)E3HUeKQ7hx0WYJWum",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "a1c39f8374bb8177b93532963f8254542a3dbe9e32b0b126182269324486c8b6"
        )))),
    },
    comment: FilterSpec {
        name: ")OgkWFXtkRBL.fntaKvbDSX(siri",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "2ea0f50afe9b98402df62c01a76f87c4e45555b29fbecec3cf96646ad75aec63"
        )))),
    },
    collective: FilterSpec {
        name: "aIKY(zS3d5vv.dREz*",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "c5867924d68836e6f4fbccf7b5023d8661f3c88542d8c7659ce4a484411b1dd5"
        )))),
    },
    info: FilterSpec {
        name: "S9)8yVzYJPOeK*_4YAeTUD",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "48e85b468392e8b73caf37fbc7924671f155acc7cd156caed585a3b8287c2072"
        )))),
    },
    question: FilterSpec {
        name: "Csa39LsSH3q3d26SekiZhUTEepAsWwXYs(tUH*DF.J-q)D4l0fbs-z*",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "2ab30cca11aed2e7b1d86e78ef37d43c46e8fd0b180f34ced7fb0bfd0b74d35d"
        )))),
    },
    revision: FilterSpec {
        name: "S0LW7y9G39VsSNOC*FQvB9",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "b75984a178edc4d93015796c119977be76d22169cb62fabf756546cc15076189"
        )))),
    },
    tag: FilterSpec {
        name: ")pmU07Nl*8faPpF39V*n6)ID",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "501effc565f1625ba162091159d277a4219900d71bb5e23edf19dbddb3afb22a"
        )))),
    },
    tag_synonym: FilterSpec {
        name: "a_LjP(LbK3SaJfWuW5",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "f3f506ac1a5faa2b517a66d8e71ddfbe9ac6263284b5f3a610446b24949566f8"
        )))),
    },
    tag_wiki: FilterSpec {
        name: "SIUtpsPZ-9l)fUK)qR2IvD",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "2ceb4965724c7118f0f03537c57498b11950de7e1641b0219613aac02d150c6a"
        )))),
    },
    user: FilterSpec {
        name: ")4lk-t8lU0Au6PWx0o1erLmkS(mCImce",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "48e8751a0a138a50f3404f40d7abe59e6933bbc87e95ba903ffca0677b101409"
        )))),
    },
};
