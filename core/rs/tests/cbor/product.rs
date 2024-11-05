mod sample {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct Sample {
        a: u32,
        b: Vec<u64>,
    }

    #[test]
    fn sample() {
        test_success(
            Sample {
                a: 1,
                b: Vec::from([2, 3]),
            },
            include_bytes!("./data/prod_sample_0.bin"),
        )
    }
    #[test]
    fn sample_bound() {
        test_success(
            Sample {
                a: u32::MAX,
                b: Vec::from([0, 1, u64::MAX]),
            },
            include_bytes!("./data/prod_sample_max.bin"),
        )
    }
}

mod normal {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct Normal(i32, Vec<i32>, Vec<u16>);

    #[test]
    fn t0() {
        test_success(
            Normal(1, [2, 3].into(), [4, 5].into()),
            include_bytes!("./data/prod_normal_0.bin"),
        )
    }
    #[test]
    fn bound() {
        test_success(
            Normal(i32::MIN, [0, i32::MAX, 10].into(), [1, 2, 3].into()),
            include_bytes!("./data/prod_normal_bound.bin"),
        )
    }
}

mod sort {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct Sort {
        a: u8,
        c: Vec<i32>,
        ab: i32,
        bac: String,
    }

    #[test]
    fn t0() {
        test_success(
            Sort {
                c: [-1, 0, 1].into(),
                bac: "example".into(),
                ab: 10,
                a: u8::MAX,
            },
            include_bytes!("./data/prod_sort_0.bin"),
        )
    }
    #[test]
    fn t1() {
        test_success(
            Sort {
                a: 1,
                c: Vec::new(),
                ab: 12,
                bac: "sss".into(),
            },
            include_bytes!("./data/prod_sort_1.bin"),
        )
    }
}

mod nested {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct Inner {
        b: u16,
        aa: bool,
    }
    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct SortNested {
        i: Inner,
        aa: u32,
        cab: i64,
    }

    #[test]
    fn t0() {
        test_success(
            SortNested {
                i: Inner { b: 10, aa: true },
                aa: 1000,
                cab: -6,
            },
            include_bytes!("./data/prod_sort_nested_0.bin"),
        )
    }
    #[test]
    fn bound() {
        test_success(
            SortNested {
                i: Inner { b: 10, aa: true },
                cab: -255,
                aa: 1000,
            },
            include_bytes!("./data/prod_sort_nested_1.bin"),
        )
    }
}

mod weird {
    use webar_core_derive::CborCodec;

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, CborCodec)]
    struct Weird {
        #[codec(rename = "\r")]
        cr: String,
        #[codec(rename = "1")]
        p1: String,
        #[codec(rename = "€")]
        euro: String,
        #[codec(rename = "דּ")]
        hebrew: String,
        #[codec(rename = "\u{80}")]
        control: String,
        #[codec(rename = "ö")]
        o: String,
        #[codec(rename = "</script>")]
        script: String,
        #[codec(rename = "😂")]
        smiley: String,
    }
    #[test]
    fn test() {
        test_success(
            Weird {
                cr: "Carriage Return".into(),
                p1: "One".into(),
                euro: "Euro Sign".into(),
                hebrew: "Hebrew Letter Dalet With Dagesh".into(),
                control: "Control".into(),
                o: "Latin Small Letter O With Diaeresis".into(),
                script: "Browser Challenge".into(),
                smiley: "Smiley".into(),
            },
            include_bytes!("./data/prod_weird.bin"),
        )
    }
}
