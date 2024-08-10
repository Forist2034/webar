use std::path::Path;

use rustix::{
    fd::{AsFd, BorrowedFd},
    fs::{self, AtFlags, OFlags},
    io::{self, Errno},
};

use crate::perm;

pub fn create_file(fd: BorrowedFd, path: &Path, data: &[u8]) -> Result<(), Errno> {
    let fd = fs::openat(
        fd,
        path,
        OFlags::CREATE | OFlags::EXCL | OFlags::WRONLY,
        perm::RO_REGULAR,
    )?;
    let mut off = 0;
    while off < data.len() {
        off += io::write(fd.as_fd(), &data[off..])?;
    }
    Ok(())
}

fn create_dir_all_at(fd: BorrowedFd, path: &Path) -> Result<(), Errno> {
    match fs::mkdirat(fd, path, perm::SHARED_DIR) {
        Ok(()) => Ok(()),
        Err(Errno::EXIST) => Ok(()),
        Err(Errno::NOENT) => {
            if let Some(p) = path.parent() {
                create_dir_all_at(fd, p)?;
            }
            fs::mkdirat(fd, path, perm::SHARED_DIR)
        }
        Err(e) => Err(e),
    }
}

pub fn create_ro_dir_at(
    fd: BorrowedFd,
    path: &Path,
    fun: impl FnOnce() -> Result<(), Errno>,
) -> Result<(), Errno> {
    match fs::mkdirat(fd, path, perm::RO_DIR_TMP) {
        Ok(()) => {
            fun()?;
            fs::chmodat(fd, path, perm::RO_DIR, AtFlags::empty())
        }
        Err(Errno::EXIST) => Ok(()),
        Err(Errno::NOENT) => {
            if let Some(p) = path.parent() {
                create_dir_all_at(fd, p)?;
            }
            fs::mkdirat(fd, path, perm::RO_DIR_TMP)?;
            fun()?;
            fs::chmodat(fd, path, perm::RO_DIR, AtFlags::empty())
        }
        Err(e) => Err(e),
    }
}
