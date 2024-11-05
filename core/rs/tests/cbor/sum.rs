mod unit {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    #[codec(rename_variants = "snake_case")]
    enum Unit {
        A,
        B,
        Var3,
    }

    #[test]
    fn a() {
        test_success(Unit::A, include_bytes!("./data/var_a.bin"))
    }
    #[test]
    fn b() {
        test_success(Unit::B, include_bytes!("./data/var_b.bin"))
    }
    #[test]
    fn var3() {
        test_success(Unit::Var3, include_bytes!("./data/var_var3.bin"))
    }
}

mod record {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    #[codec(rename_variants = "snake_case")]
    enum Record {
        V1 { a: u32, k: i16, ab: i8, c: bool },
        Sv2 { z: bool, full: bool, new: u32 },
    }

    #[test]
    fn v1() {
        test_success(
            Record::V1 {
                a: 10,
                k: -128,
                ab: -10,
                c: false,
            },
            include_bytes!("./data/var_v1.bin"),
        )
    }
    #[test]
    fn sv2() {
        test_success(
            Record::Sv2 {
                z: false,
                full: true,
                new: 0,
            },
            include_bytes!("./data/var_sv2.bin"),
        )
    }
}

mod normal {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    #[codec(rename_variants = "snake_case")]
    enum Normal {
        Tv1(u8, i16),
        Tv2(bool, String),
    }

    #[test]
    fn tv1() {
        test_success(
            Normal::Tv1(123, -3200),
            include_bytes!("./data/var_tv1.bin"),
        )
    }
    #[test]
    fn tv2() {
        test_success(
            Normal::Tv2(false, "123".into()),
            include_bytes!("./data/var_tv2.bin"),
        )
    }
}

mod unary {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    #[codec(rename_variants = "snake_case")]
    enum Unary {
        Nv(bool),
        Nv2(Vec<u32>),
    }

    #[test]
    fn nv1() {
        test_success(Unary::Nv(true), include_bytes!("./data/var_nv.bin"))
    }
    #[test]
    fn nv2() {
        test_success(
            Unary::Nv2([123, 456].into()),
            include_bytes!("./data/var_nv2.bin"),
        )
    }
}

mod mixed {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    #[codec(rename_variants = "snake_case")]
    enum Mixed {
        MvUnit,
        MvU2,
        MvTup(bool, bool),
        MvNewt(i8),
        MvStruct { c: i8, ac: bool, ab: String },
    }

    #[test]
    fn unit() {
        test_success(Mixed::MvUnit, include_bytes!("./data/var_mv_unit.bin"))
    }
    #[test]
    fn u2() {
        test_success(Mixed::MvU2, include_bytes!("./data/var_mv_u2.bin"))
    }
    #[test]
    fn tup() {
        test_success(
            Mixed::MvTup(false, true),
            include_bytes!("./data/var_mv_tup.bin"),
        )
    }
    #[test]
    fn unary() {
        test_success(Mixed::MvNewt(-10), include_bytes!("./data/var_mv_newt.bin"))
    }
    #[test]
    fn record() {
        test_success(
            Mixed::MvStruct {
                c: -1,
                ac: true,
                ab: "a".into(),
            },
            include_bytes!("./data/var_mv_struct.bin"),
        )
    }
}
