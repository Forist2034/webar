use std::time::SystemTime;

use serde::Deserialize;

use webar_data::ser::Serialize;

pub mod digest;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Timestamp {
    pub seconds: u64,
    pub nanoseconds: u32,
}
impl Timestamp {
    pub fn now() -> Self {
        Self::from(SystemTime::now())
    }
}
impl From<SystemTime> for Timestamp {
    fn from(value: SystemTime) -> Self {
        let d = value.duration_since(SystemTime::UNIX_EPOCH).unwrap();
        Self {
            seconds: d.as_secs(),
            nanoseconds: d.subsec_nanos(),
        }
    }
}
