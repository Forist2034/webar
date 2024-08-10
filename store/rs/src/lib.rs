mod perm {
    use rustix::fs::Mode;

    pub const IGNORE: Mode = Mode::empty();
    /// set mode as umask
    pub const SHARED_DIR: Mode = Mode::from_raw_mode(0o777);

    // not used
    // pub const RW_REGULAR: Mode = Mode::from_raw_mode(0o666);
    pub const RO_REGULAR: Mode = Mode::from_raw_mode(0o444);

    pub const RO_DIR: Mode = Mode::from_raw_mode(0o555);
    /// temp mode when creating readonly directory
    pub const RO_DIR_TMP: Mode = Mode::from_raw_mode(0o755);
}

pub mod blob;
pub mod object;

mod utils;
