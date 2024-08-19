use std::fmt::Debug;

use webar_data::{json, ser::Serialize};

#[path = "../common/mod.rs"]
mod common;

fn test_serde<D: Debug + Eq + Serialize + serde::de::DeserializeOwned>(data: D, bin: &str) {
    assert_eq!(json::to_string(&data).unwrap(), bin, "serialize");
    assert_eq!(serde_json::from_str::<D>(bin).unwrap(), data, "deserialize");
}

mod min_max {
    use crate::test_serde;

    macro_rules! test_int {
        ($t:ident) => {
            mod $t {
                #[test]
                fn min() {
                    $crate::test_serde(
                        $t::MIN,
                        include_str!(concat!("./data/", stringify!($t), "_min.json")),
                    )
                }
                #[test]
                fn max() {
                    $crate::test_serde(
                        $t::MAX,
                        include_str!(concat!("./data/", stringify!($t), "_max.json")),
                    )
                }
            }
        };
    }

    test_int!(i8);
    #[test]
    fn u8_max() {
        test_serde(u8::MAX, include_str!("./data/u8_max.json"));
    }

    test_int!(i16);
    #[test]
    fn u16_max() {
        test_serde(u16::MAX, include_str!("./data/u16_max.json"))
    }

    test_int!(i32);
    #[test]
    fn u32_max() {
        test_serde(u32::MAX, include_str!("./data/u32_max.json"))
    }

    test_int!(i64);
    #[test]
    fn u64_max() {
        test_serde(u64::MAX, include_str!("./data/u64_max.json"))
    }
}

mod text {
    fn test_text(s: &str, bin: &str) {
        crate::test_serde(String::from(s), bin)
    }

    #[test]
    fn empty() {
        test_text("", include_str!("./data/empty_str.json"))
    }

    #[test]
    fn example() {
        test_text("example string", include_str!("./data/str.json"))
    }

    #[test]
    fn unnormalized_str() {
        test_text("Å", include_str!("./data/unnormalized_str.json"))
    }

    #[test]
    fn ch_str() {
        test_text("水", include_str!("./data/ch_str.json"))
    }

    #[test]
    fn geek_str() {
        test_text("𐅑", include_str!("./data/geek_str.json"))
    }

    #[test]
    fn escape() {
        test_text(
            "€$\u{000f}\nA'B\"\\\\\"/",
            include_str!("./data/escape_str.json"),
        )
    }
}

mod array {
    use std::fmt::Debug;

    fn test_array<const N: usize, D>(data: [D; N], bin: &str)
    where
        D: Debug + Eq + webar_data::ser::Serialize + serde::de::DeserializeOwned,
    {
        crate::test_serde(Vec::from(data), bin)
    }

    #[test]
    fn empty() {
        test_array::<0, i32>([], include_str!("./data/empty_arr.json"))
    }
    #[test]
    fn array_123() {
        test_array([1u32, 2, 3], include_str!("./data/123_arr.json"))
    }
}

mod product {
    use crate::common::product;

    mod sample {
        use crate::{common::product::Sample, test_serde};

        #[test]
        fn sample_0() {
            test_serde(Sample::sample(), include_str!("./data/prod_sample_0.json"))
        }
        #[test]
        fn bound() {
            test_serde(
                Sample::sample_bound(),
                include_str!("./data/prod_sample_max.json"),
            )
        }
    }

    mod tuple {
        use crate::{common::product::Tuple, test_serde};

        #[test]
        fn t0() {
            test_serde(Tuple::t0(), include_str!("./data/prod_normal_0.json"))
        }
        #[test]
        fn bound() {
            test_serde(
                Tuple::bound(),
                include_str!("./data/prod_normal_bound.json"),
            )
        }
    }

    mod sort {
        use crate::{common::product::Sorted, test_serde};

        #[test]
        fn t0() {
            test_serde(Sorted::t0(), include_str!("./data/prod_sort_0.json"))
        }
        #[test]
        fn t1() {
            test_serde(Sorted::t1(), include_str!("./data/prod_sort_1.json"))
        }
    }

    mod sort_nested {
        use crate::{common::product::SortNested, test_serde};

        #[test]
        fn t0() {
            test_serde(
                SortNested::T0,
                include_str!("./data/prod_sort_nested_0.json"),
            )
        }
        #[test]
        fn bound() {
            test_serde(
                SortNested::BOUND,
                include_str!("./data/prod_sort_nested_1.json"),
            )
        }
    }

    #[test]
    fn unsorted() {
        crate::test_serde(
            product::ProdUnsorted::T0,
            include_str!("./data/prod_unsorted_0.json"),
        )
    }

    #[test]
    fn weird() {
        crate::test_serde(
            product::Weird::test(),
            include_str!("./data/prod_weird.json"),
        )
    }
}

mod sum {
    mod unit {
        use crate::{common::sum::Unit, test_serde};

        #[test]
        fn var_a() {
            test_serde(Unit::A, include_str!("./data/var_a.json"))
        }
        #[test]
        fn var_b() {
            test_serde(Unit::BB, include_str!("./data/var_b.json"))
        }
        #[test]
        fn var_3() {
            test_serde(Unit::Variant3, include_str!("./data/var_var3.json"))
        }
    }

    mod struct_v {
        use crate::{common::sum::Struct, test_serde};

        #[test]
        fn var_v1() {
            test_serde(Struct::T_V1, include_str!("./data/var_v1.json"))
        }
        #[test]
        fn var_v2() {
            test_serde(Struct::T_V2, include_str!("./data/var_sv2.json"))
        }
    }

    mod tuple {
        use crate::{common::sum::Tuple, test_serde};

        #[test]
        fn var_tv1() {
            test_serde(Tuple::T_V1, include_str!("./data/var_tv1.json"))
        }
        #[test]
        fn var_tv2() {
            test_serde(Tuple::tv2(), include_str!("./data/var_tv2.json"))
        }
    }

    mod unary {
        use crate::{common::sum::Unary, test_serde};

        #[test]
        fn var_nv1() {
            test_serde(Unary::T_V1, include_str!("./data/var_nv.json"))
        }
        #[test]
        fn var_nv2() {
            test_serde(Unary::nv2(), include_str!("./data/var_nv2.json"))
        }
    }

    mod unsorted {
        use crate::{common::sum::Unsorted, test_serde};

        #[test]
        fn unit() {
            test_serde(Unsorted::T_UNIT, include_str!("./data/var_sus_unit.json"))
        }
        #[test]
        fn unary() {
            test_serde(Unsorted::T_UNARY, include_str!("./data/var_sus_unary.json"))
        }
        #[test]
        fn record1() {
            test_serde(
                Unsorted::t_record1(),
                include_str!("./data/var_sus_record1.json"),
            )
        }
        #[test]
        fn tuple() {
            test_serde(Unsorted::T_TUPLE, include_str!("./data/var_sus_tuple.json"))
        }
        #[test]
        fn record2() {
            test_serde(
                Unsorted::T_RECORD2,
                include_str!("./data/var_sus_record2.json"),
            )
        }
    }

    mod mixed {
        use crate::{common::sum::Mixed, test_serde};

        #[test]
        fn var_unit() {
            test_serde(Mixed::MvUnit, include_str!("./data/var_mv_unit.json"))
        }
        #[test]
        fn var_u2() {
            test_serde(Mixed::MvUnit2, include_str!("./data/var_mv_u2.json"))
        }
        #[test]
        fn var_tup() {
            test_serde(Mixed::T_TUP, include_str!("./data/var_mv_tup.json"))
        }
        #[test]
        fn var_unary() {
            test_serde(Mixed::T_UNARY, include_str!("./data/var_mv_newt.json"))
        }
        #[test]
        fn var_struct() {
            test_serde(Mixed::t_struct(), include_str!("./data/var_mv_struct.json"))
        }
    }
}

mod uuid {
    use crate::{common::uuid, test_serde};

    #[test]
    fn nil() {
        test_serde(uuid::NIL, include_str!("./data/uuid_nil.json"))
    }
    #[test]
    fn t1() {
        test_serde(uuid::T1, include_str!("./data/uuid_1.json"))
    }
}

mod set {
    use crate::{common::set, test_serde};

    #[test]
    fn empty() {
        test_serde(set::empty(), include_str!("./data/set_empty.json"))
    }
    #[test]
    fn example() {
        test_serde(set::example(), include_str!("./data/set_example.json"))
    }
}

mod map {
    use crate::{common::map, test_serde};

    #[test]
    fn empty() {
        test_serde(map::empty(), include_str!("./data/map_empty.json"))
    }
    #[test]
    fn example() {
        test_serde(map::example(), include_str!("./data/map_example.json"))
    }
}
