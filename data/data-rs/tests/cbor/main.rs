use std::fmt::Debug;

use webar_data::{cbor::to_vec, ser::Serialize};

#[path = "../common/mod.rs"]
mod common;

fn test_serde<D: Serialize + serde::de::DeserializeOwned + Eq + Debug>(data: D, bin: &[u8]) {
    assert_eq!(to_vec(&data), bin, "serialize");
    assert_eq!(
        ciborium::from_reader::<D, _>(bin).unwrap(),
        data,
        "deserialize"
    );
}

mod positive {
    use crate::test_serde;

    #[test]
    fn zero() {
        test_serde(0u8, include_bytes!("./data/zero.bin"))
    }

    #[test]
    fn zero_u64() {
        test_serde(0u64, include_bytes!("./data/zero.bin"))
    }

    #[test]
    fn one_u32() {
        test_serde(1u32, include_bytes!("./data/one.bin"))
    }

    #[test]
    fn ten_i32() {
        test_serde(10i32, include_bytes!("./data/ten.bin"))
    }

    #[test]
    fn twenty_three_u8() {
        test_serde(23u8, include_bytes!("./data/23.bin"))
    }

    #[test]
    fn twenty_four_u64() {
        test_serde(24u64, include_bytes!("./data/24.bin"))
    }

    #[test]
    fn hundred_i8() {
        test_serde(100i8, include_bytes!("./data/100.bin"))
    }

    #[test]
    fn thousand_u16() {
        test_serde(1000u16, include_bytes!("./data/1000.bin"))
    }

    #[test]
    fn million_u32() {
        test_serde(1_000_000u32, include_bytes!("./data/million.bin"))
    }

    #[test]
    fn test_1e12_i64() {
        test_serde(1_000_000_000_000i64, include_bytes!("./data/1e12.bin"))
    }
}

mod negative {
    use crate::test_serde;

    #[test]
    fn minus_one_i8() {
        test_serde(-1i8, include_bytes!("./data/minus_one.bin"))
    }

    #[test]
    fn minus_ten_i16() {
        test_serde(-10i16, include_bytes!("./data/minus_ten.bin"))
    }

    #[test]
    fn minus_hundred() {
        test_serde(-100i32, include_bytes!("./data/-100.bin"))
    }

    #[test]
    fn minus_thousand() {
        test_serde(-1000i16, include_bytes!("./data/-1000.bin"))
    }
}

mod max_min {
    use crate::test_serde;

    #[test]
    fn i64_max() {
        test_serde(i64::MAX, include_bytes!("./data/i64_max.bin"))
    }
    #[test]
    fn i64_min() {
        test_serde(i64::MIN, include_bytes!("./data/i64_min.bin"))
    }

    #[test]
    fn u64_max() {
        test_serde(u64::MAX, include_bytes!("./data/u64_max.bin"))
    }

    #[test]
    fn u8_max() {
        test_serde(u8::MAX, include_bytes!("./data/u8_max.bin"))
    }

    #[test]
    fn i8_max() {
        test_serde(i8::MAX, include_bytes!("./data/i8_max.bin"))
    }

    #[test]
    fn i8_min() {
        test_serde(i8::MIN, include_bytes!("./data/i8_min.bin"))
    }

    #[test]
    fn u16_max() {
        test_serde(u16::MAX, include_bytes!("./data/u16_max.bin"))
    }

    #[test]
    fn i16_max() {
        test_serde(i16::MAX, include_bytes!("./data/i16_max.bin"))
    }

    #[test]
    fn i16_min() {
        test_serde(i16::MIN, include_bytes!("./data/i16_min.bin"))
    }

    #[test]
    fn u32_max() {
        test_serde(u32::MAX, include_bytes!("./data/u32_max.bin"))
    }

    #[test]
    fn i32_max() {
        test_serde(i32::MAX, include_bytes!("./data/i32_max.bin"))
    }

    #[test]
    fn i32_min() {
        test_serde(i32::MIN, include_bytes!("./data/i32_min.bin"))
    }
}

#[test]
fn test_false() {
    test_serde(false, include_bytes!("./data/false.bin"))
}

#[test]
fn test_true() {
    test_serde(true, include_bytes!("./data/true.bin"))
}

mod text {
    fn test_serde<'a>(data: &'a str, bin: &'a [u8]) {
        crate::test_serde(String::from(data), bin)
    }

    #[test]
    fn empty() {
        test_serde("", include_bytes!("./data/empty_str.bin"))
    }

    #[test]
    fn a() {
        test_serde("a", include_bytes!("./data/a.bin"))
    }

    #[test]
    fn ietf() {
        test_serde("IETF", include_bytes!("./data/ietf.bin"))
    }

    #[test]
    fn escape() {
        test_serde("\"\\", include_bytes!("./data/escape_str.bin"))
    }

    #[test]
    fn u_str() {
        test_serde("ü", include_bytes!("./data/u_str.bin"))
    }

    #[test]
    fn ch_str() {
        test_serde("水", include_bytes!("./data/ch_str.bin"))
    }

    #[test]
    fn geek_str() {
        test_serde("𐅑", include_bytes!("./data/geek_str.bin"))
    }

    #[test]
    fn large() {
        test_serde(
            include_str!("./data/large_text.txt"),
            include_bytes!("./data/large_text.bin"),
        )
    }
}

mod array {
    use std::fmt::Debug;

    fn test_serde<const N: usize, I>(data: [I; N], bin: &[u8])
    where
        I: webar_data::ser::Serialize + Debug + Eq + serde::de::DeserializeOwned,
    {
        crate::test_serde(Vec::from(data), bin)
    }

    #[test]
    fn empty() {
        test_serde::<0, u8>([], include_bytes!("./data/empty_array.bin"))
    }

    #[test]
    fn array_123() {
        test_serde([1, 2, 3], include_bytes!("./data/123_array.bin"))
    }

    #[test]
    fn array_25() {
        test_serde(
            [
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25,
            ],
            include_bytes!("./data/array_25.bin"),
        )
    }
}

mod bytes {
    fn test_serde(data: &[u8], bin: &[u8]) {
        crate::test_serde(webar_data::bytes::ByteBuf(Vec::from(data)), bin)
    }

    #[test]
    fn empty() {
        test_serde(&[], include_bytes!("./data/empty_bytes.bin"))
    }

    #[test]
    fn sample() {
        test_serde(
            &hex_literal::hex!("01020304"),
            include_bytes!("./data/sample_bytes.bin"),
        )
    }
}

mod struct_t {
    use serde::Deserialize;
    use webar_derive::Serialize;

    use crate::test_serde;

    mod sample {
        use crate::{common::product::Sample, test_serde};

        #[test]
        fn sample_0() {
            test_serde(Sample::sample(), include_bytes!("./data/prod_sample_0.bin"))
        }
        #[test]
        fn bound() {
            test_serde(
                Sample::sample_bound(),
                include_bytes!("./data/prod_sample_max.bin"),
            )
        }
    }

    mod tuple {
        use crate::{common::product::Tuple, test_serde};

        #[test]
        fn t0() {
            test_serde(Tuple::t0(), include_bytes!("./data/prod_normal_0.bin"))
        }
        #[test]
        fn bound() {
            test_serde(
                Tuple::bound(),
                include_bytes!("./data/prod_normal_bound.bin"),
            )
        }
    }

    mod sort {
        use crate::{common::product::Sorted, test_serde};

        #[test]
        fn t0() {
            test_serde(Sorted::t0(), include_bytes!("./data/prod_sort_0.bin"))
        }
        #[test]
        fn t1() {
            test_serde(Sorted::t1(), include_bytes!("./data/prod_sort_1.bin"))
        }
    }

    mod sort_nested {
        use crate::{common::product::SortNested, test_serde};

        #[test]
        fn t0() {
            test_serde(
                SortNested::T0,
                include_bytes!("./data/prod_sort_nested_0.bin"),
            )
        }
        #[test]
        fn bound() {
            test_serde(
                SortNested::BOUND,
                include_bytes!("./data/prod_sort_nested_1.bin"),
            )
        }
    }

    #[test]
    fn nested() {
        #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
        struct Inner {
            b: String,
        }
        #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
        struct Outer(String, Inner);
        test_serde(
            Outer(
                String::from("a"),
                Inner {
                    b: String::from("c"),
                },
            ),
            include_bytes!("./data/nested_struct.bin"),
        )
    }

    #[test]
    fn renamed() {
        #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
        struct Renamed {
            k: i32,
            #[serde(rename = "a")]
            aa: u32,
            c: bool,
        }
        test_serde(
            Renamed {
                k: -1,
                aa: 0,
                c: false,
            },
            include_bytes!("./data/struct_renamed.bin"),
        )
    }
}

mod variant {

    mod unit_variant {
        use crate::common::sum::Unit;

        #[test]
        fn variant_a() {
            crate::test_serde(Unit::A, include_bytes!("./data/var_a.bin"))
        }
        #[test]
        fn variant_b() {
            crate::test_serde(Unit::BB, include_bytes!("./data/var_b.bin"))
        }
        #[test]
        fn variant_3() {
            crate::test_serde(Unit::Variant3, include_bytes!("./data/var_var3.bin"))
        }
    }

    mod struct_variant {
        use crate::{common::sum::Struct, test_serde};

        #[test]
        fn var_v1() {
            test_serde(Struct::T_V1, include_bytes!("./data/var_v1.bin"))
        }
        #[test]
        fn var_v2() {
            test_serde(Struct::T_V2, include_bytes!("./data/var_sv2.bin"))
        }
    }

    mod tuple_variant {
        use crate::{common::sum::Tuple, test_serde};

        #[test]
        fn var_tv1() {
            test_serde(Tuple::T_V1, include_bytes!("./data/var_tv1.bin"))
        }
        #[test]
        fn var_tv2() {
            test_serde(Tuple::tv2(), include_bytes!("./data/var_tv2.bin"))
        }
    }

    mod newtype_variant {
        use crate::{common::sum::Unary, test_serde};

        #[test]
        fn var_nv1() {
            test_serde(Unary::T_V1, include_bytes!("./data/var_nv.bin"))
        }
        #[test]
        fn var_nv2() {
            test_serde(Unary::nv2(), include_bytes!("./data/var_nv2.bin"))
        }
    }

    mod mixed_variant {
        use crate::{common::sum::Mixed, test_serde};

        #[test]
        fn var_unit() {
            test_serde(Mixed::MvUnit, include_bytes!("./data/var_mv_unit.bin"))
        }
        #[test]
        fn var_u2() {
            test_serde(Mixed::MvUnit2, include_bytes!("./data/var_mv_u2.bin"))
        }
        #[test]
        fn var_tup() {
            test_serde(Mixed::T_TUP, include_bytes!("./data/var_mv_tup.bin"))
        }
        #[test]
        fn var_newt() {
            test_serde(Mixed::T_UNARY, include_bytes!("./data/var_mv_newt.bin"))
        }
        #[test]
        fn var_struct() {
            test_serde(
                Mixed::t_struct(),
                include_bytes!("./data/var_mv_struct.bin"),
            )
        }
        #[test]
        fn var_rename() {
            test_serde(
                Mixed::MvRename {
                    c: true,
                    k: 10,
                    ds: 0,
                },
                include_bytes!("./data/var_mv_rename.bin"),
            )
        }
    }
}

mod uuid {
    use crate::{common::uuid, test_serde};

    #[test]
    fn nil() {
        test_serde(uuid::NIL, include_bytes!("./data/uuid_nil.bin"))
    }

    #[test]
    fn t1() {
        test_serde(uuid::T1, include_bytes!("./data/uuid_1.bin"))
    }
}

mod set {
    use crate::{common::set, test_serde};

    #[test]
    fn empty() {
        test_serde(set::empty(), include_bytes!("./data/set_empty.bin"))
    }
    #[test]
    fn example() {
        test_serde(set::example(), include_bytes!("./data/set_example.bin"))
    }
}

mod map {
    use crate::{common::map, test_serde};

    #[test]
    fn empty() {
        test_serde(map::empty(), include_bytes!("./data/map_empty.bin"))
    }
    #[test]
    fn example() {
        test_serde(map::example(), include_bytes!("./data/map_example.bin"))
    }
}
