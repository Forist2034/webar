use hex_literal::hex;

use webar_core::digest::{Digest, Sha256};
use webar_stackexchange_core::filter::{FilterId, FilterInfo, TypeMap};

pub const HS_FILTER_INFO: TypeMap<FilterInfo<&str>> = TypeMap {
    answer: FilterInfo {
        name: ".BcrXh4RU(M_r5Qrar94ww5zJfMO32fPy",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "df9ae3eb7121f6317ec2e288bae43fcf6c2171cecaa7fef02a89b737dd720695"
        )))),
    },
    badge: FilterInfo {
        name: "3pH)E3HUeKQ7hx0WYJWum",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "3dc5100f1a84a33561fbe0b2fec092dc8301dec73126b235a165f1550f49db75"
        )))),
    },
    comment: FilterInfo {
        name: ")OgkWFXtkRBL.fntaKvbDSX(siri",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "9a4dd62331d6cef8219b3c6634754ec72d3225cada8a4af8c90f4f6ce3acc46d"
        )))),
    },
    collective: FilterInfo {
        name: "aIKY(zS3d5vv.dREz*",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "ca07af06f0675d22279846ace09152b9397c3cf0611cf273eda50748ef589adf"
        )))),
    },
    info: FilterInfo {
        name: "1uatltI)ObcWErUeHc(SvZuwP",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "c0e967adb5e35ce0588069d456843887af2158f168c418facf927958aea2c3ff"
        )))),
    },
    question: FilterInfo {
        name: "Csa39LsSH3q3d26SekiZhUTEepAsWwXYs(tUH*DF.J-q)D4l0fbs-z*",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "2f1e3d25e7dc0760cc4a33b3283430b0e0a0ffecf802660ee0768798f4e2daca"
        )))),
    },
    revision: FilterInfo {
        name: "S0LW7y9G39VsSNOC*FQvB9",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "f9ef9b135297b25e8002325d0372fc84ac4ea059702a4060a2cae3154e5f1400"
        )))),
    },
    tag: FilterInfo {
        name: ")pmU07Nl*8faPpF39V*n6)ID",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "c96a0bfaf1f01d6a0852be89a6bf9c4cce8e2673377f0265684eda8bb8bc7f0a"
        )))),
    },
    tag_synonym: FilterInfo {
        name: "a_LjP(LbK3SaJfWuW5",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "9f79777a40795d6d5abf986bff3fcc7d8b1fc7b03028122a506972e87b1a7e41"
        )))),
    },
    tag_wiki: FilterInfo {
        name: "1w_l8s9RGuxzc_RNLKFY9)7oP",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "97e677fbbc625cd2633e1c34e17a154fbca88d9f8034577899ceb1af611f5c72"
        )))),
    },
    user: FilterInfo {
        name: ")4lk-t8lU0Au6PWx0o1erLmkS(mCImce",
        id: FilterId(Digest::Sha256(Sha256(hex!(
            "5de5a1517992cb7999d9ae988c163ff9e7a88aae704a847e2c1ec5c94ad51cb7"
        )))),
    },
};
