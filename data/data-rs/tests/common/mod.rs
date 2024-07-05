pub mod product {
    use serde::Deserialize;
    use webar_derive::Serialize;

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Sample {
        a: u32,
        b: Vec<u64>,
    }
    impl Sample {
        pub fn sample() -> Self {
            Sample {
                a: 1,
                b: Vec::from([2, 3]),
            }
        }
        pub fn sample_bound() -> Self {
            Sample {
                a: u32::MAX,
                b: Vec::from([0, 1, u64::MAX]),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Tuple(i32, Vec<i32>, Vec<u16>);
    impl Tuple {
        pub fn t0() -> Self {
            Self(1, Vec::from([2, 3]), Vec::from([4, 5]))
        }
        pub fn bound() -> Self {
            Self(i32::MIN, Vec::from([0, i32::MAX, 10]), Vec::from([1, 2, 3]))
        }
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Sorted {
        a: u8,
        c: Vec<i32>,
        ab: i32,
        bac: String,
    }
    impl Sorted {
        pub fn t0() -> Self {
            Self {
                a: u8::MAX,
                c: Vec::from([-1, 0, 1]),
                ab: 10,
                bac: String::from("example"),
            }
        }
        pub fn t1() -> Self {
            Self {
                a: 1,
                c: Vec::new(),
                ab: 12,
                bac: String::from("sss"),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    struct SortInner {
        b: u16,
        aa: bool,
    }
    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct SortNested {
        i: SortInner,
        aa: u32,
        cab: i64,
    }
    impl SortNested {
        pub const T0: Self = Self {
            i: SortInner { b: 10, aa: true },
            aa: 1000,
            cab: -6,
        };

        pub const BOUND: Self = Self {
            i: SortInner { b: 10, aa: true },
            aa: 1000,
            cab: -255,
        };
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Weird {
        #[serde(rename = "1")]
        one: String,
        #[serde(rename = "\r")]
        cr: String,
        #[serde(rename = "</script>")]
        script: String,
        #[serde(rename = "\u{0080}")]
        control: String,
        #[serde(rename = "ö")]
        o: String,
        #[serde(rename = "😂")]
        smiley: String,
        #[serde(rename = "€")]
        euro: String,
        #[serde(rename = "דּ")]
        hebrew: String,
    }
    impl Weird {
        pub fn test() -> Self {
            Self {
                one: "One".into(),
                cr: "Carriage Return".into(),
                script: "Browser Challenge".into(),
                control: "Control".into(),
                o: "Latin Small Letter O With Diaeresis".into(),
                smiley: "Smiley".into(),
                euro: "Euro Sign".into(),
                hebrew: "Hebrew Letter Dalet With Dagesh".into(),
            }
        }
    }
}

pub mod sum {
    use serde::Deserialize;
    use webar_derive::Serialize;

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Unit {
        #[serde(rename = "a")]
        A,
        #[serde(rename = "b")]
        BB,
        #[serde(rename = "var3")]
        Variant3,
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Struct {
        #[serde(rename = "v1")]
        V1 { a: u32, k: i16, ab: i8, c: bool },
        #[serde(rename = "sv2")]
        V2 { z: bool, full: bool, new: u32 },
    }
    impl Struct {
        pub const T_V1: Self = Self::V1 {
            a: 10,
            k: -128,
            ab: -10,
            c: false,
        };
        pub const T_V2: Self = Self::V2 {
            z: false,
            full: true,
            new: 0,
        };
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Tuple {
        #[serde(rename = "tv1")]
        TV1(u8, i16),
        #[serde(rename = "tv2")]
        TV2(bool, String),
    }
    impl Tuple {
        pub const T_V1: Self = Self::TV1(123, -3200);
        pub fn tv2() -> Self {
            Self::TV2(false, String::from("123"))
        }
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Unary {
        #[serde(rename = "nv")]
        Nv1(bool),
        #[serde(rename = "nv2")]
        Nv2(Vec<u32>),
    }
    impl Unary {
        pub const T_V1: Self = Self::Nv1(true);
        pub fn nv2() -> Self {
            Self::Nv2(Vec::from([123, 456]))
        }
    }

    #[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Mixed {
        #[serde(rename = "mv_unit")]
        MvUnit,
        #[serde(rename = "mv_u2")]
        MvUnit2,
        #[serde(rename = "mv_tup")]
        MvTup(bool, bool),
        #[serde(rename = "mv_newt")]
        MvNewType(i8),
        #[serde(rename = "mv_struct")]
        MvStruct { c: i8, ac: bool, ab: String },
        #[serde(rename = "mv_rename")]
        MvRename {
            c: bool,
            #[serde(rename = "a")]
            k: u64,
            ds: u32,
        },
    }
    impl Mixed {
        pub const T_TUP: Self = Self::MvTup(false, true);
        pub const T_UNARY: Self = Self::MvNewType(-10);
        pub fn t_struct() -> Self {
            Self::MvStruct {
                c: -1,
                ac: true,
                ab: String::from("a"),
            }
        }
    }
}

pub mod uuid {
    use uuid::Uuid;

    pub const NIL: Uuid = Uuid::nil();
    pub const T1: Uuid = uuid::uuid!("c2cc10e1-57d6-4b6f-9899-38d972112d8c");
}

pub mod set {
    use std::collections::BTreeSet;

    pub fn empty() -> BTreeSet<i32> {
        BTreeSet::new()
    }
    pub fn example() -> BTreeSet<i32> {
        BTreeSet::from([1, 2, 3])
    }
}

pub mod map {
    use std::collections::BTreeMap;

    pub fn empty() -> BTreeMap<u8, bool> {
        BTreeMap::new()
    }
    pub fn example() -> BTreeMap<u8, bool> {
        BTreeMap::from([(1, true), (2, false), (3, true)])
    }
}
