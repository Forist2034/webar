use hex_literal::hex;

use webar_core::digest::{Digest, Sha256};
use webar_stackexchange_core::api::filter::{FilterId, FilterSpec, TypeMap};

pub const HS_FILTER_INFO: TypeMap<FilterSpec<&str>> = TypeMap {
    answer: FilterSpec {
        name: ".BcrXh4RU(M_r5Qrar94ww5zJfMO32fPy",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "7de7f9f4017db2cbe9c4404ca97ea711b144d6739bdbdaf524dd087b099fb02d"
        )))),
    },
    badge: FilterSpec {
        name: "3pH)E3HUeKQ7hx0WYJWum",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "cc5f7146b570ec0535fd251290a189b2844c9f6a754405df8729582aefcb596c"
        )))),
    },
    comment: FilterSpec {
        name: ")OgkWFXtkRBL.fntaKvbDSX(siri",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "5824f89f0ec4527664b740daa407d096ad8d5838964044d5625d3960c14e9053"
        )))),
    },
    collective: FilterSpec {
        name: "aIKY(zS3d5vv.dREz*",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "b0eeb6b1c1d2aa88efc897928fecaf5ccabcca307d561f4f907e1249f5ba5989"
        )))),
    },
    info: FilterSpec {
        name: "S9)8yVzYJPOeK*_4YAeTUD",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "a4845bb12284db8aff025d1b0536a23df59fb351a93dd4222fee19c583786f56"
        )))),
    },
    question: FilterSpec {
        name: "Csa39LsSH3q3d26SekiZhUTEepAsWwXYs(tUH*DF.J-q)D4l0fbs-z*",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "025bcb3e389a42842c78830cc5692cb2e71aeb0ed86378754cdcea9d80b11561"
        )))),
    },
    revision: FilterSpec {
        name: "S0LW7y9G39VsSNOC*FQvB9",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "b2356745a4554793c78f94754bcf9fa3843d97c70df1b47228211b0d9160666e"
        )))),
    },
    tag: FilterSpec {
        name: ")pmU07Nl*8faPpF39V*n6)ID",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "0b7a3175a6e93a29b666546efede702ad49f25da2eb8308d6af59e6c478b48fc"
        )))),
    },
    tag_synonym: FilterSpec {
        name: "a_LjP(LbK3SaJfWuW5",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "998864966351e5dbf6512333cb9cf600e4da46fb949f487a4129fd0449d58311"
        )))),
    },
    tag_wiki: FilterSpec {
        name: "SIUtpsPZ-9l)fUK)qR2IvD",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "bdda284634f7c710b6abb7e2a0f618867d2d02c0399fa1c3f16fbfa440edae82"
        )))),
    },
    user: FilterSpec {
        name: ")4lk-t8lU0Au6PWx0o1erLmkS(mCImce",
        id: FilterId::new(Digest::Sha256(Sha256(hex!(
            "d1a4bd617173bf05b7bc4ef9c8c45e84f10c0c20f722b82025ee403f45b66f67"
        )))),
    },
};
