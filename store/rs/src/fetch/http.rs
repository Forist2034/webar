use std::{ffi::CStr, io::Read, marker::PhantomData};

use rustix::{
    fd::BorrowedFd,
    fs::{self, OFlags},
};

use webar_core::{
    digest::{Digest, Sha256},
    fetch::http::{
        internal::DigestField, FetchId, FetchInfo, FetchMeta, Traffic, DATA_FILE, KEY_LOG_FILE,
        LOG_FILE, META_FILE, REQUEST_META_FILE, WIRESHARK_DATA_FILE,
    },
};
use webar_data::ser::Serialize;

use crate::{blob, object, perm};

fn hash_file<F>(root: BorrowedFd, file: &CStr) -> Result<DigestField<F>, std::io::Error> {
    struct HashWriter(sha2::Sha256);
    impl std::io::Write for HashWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            sha2::Digest::update(&mut self.0, buf);
            Ok(buf.len())
        }
        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
            sha2::Digest::update(&mut self.0, buf);
            Ok(())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    let mut writer = HashWriter(sha2::Digest::new());
    std::io::copy(
        &mut std::fs::File::from(fs::openat(root, file, OFlags::RDONLY, perm::IGNORE)?),
        &mut writer,
    )?;
    Ok(DigestField(
        Digest::Sha256(Sha256(sha2::Digest::finalize(writer.0).into())),
        PhantomData,
    ))
}

fn read_file(root: BorrowedFd, file: &CStr) -> Result<Vec<u8>, std::io::Error> {
    let fd = fs::openat(root, file, OFlags::RDONLY, perm::IGNORE)?;
    let mut buf = Vec::new();
    std::fs::File::from(fd).read_to_end(&mut buf)?;
    Ok(buf)
}

#[derive(Debug, thiserror::Error)]
enum InnerError {
    #[error("io error")]
    Io(
        #[source]
        #[from]
        std::io::Error,
    ),
    #[error("failed to decode metadata")]
    Decode(#[source] ciborium::de::Error<std::io::Error>),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error(InnerError);

fn add_fetch<L, H, A, S, R>(
    is_wireshark: bool,
    obj_store: &object::WebsiteStore<H, A, S, R>,
    root: BorrowedFd,
    ty: R,
) -> Result<FetchId<L>, InnerError>
where
    L: Serialize + serde::de::DeserializeOwned,
    H: Serialize + Copy,
    A: Serialize,
    S: Serialize,
    R: Serialize,
{
    let meta: FetchMeta<L> = ciborium::from_reader(read_file(root, META_FILE.c_path)?.as_slice())
        .map_err(InnerError::Decode)?;
    let traffic = if is_wireshark {
        Traffic::Wireshark {
            key_log: hash_file(root, KEY_LOG_FILE.c_path)?,
            request_meta: hash_file(root, REQUEST_META_FILE.c_path)?,
            data: hash_file(root, WIRESHARK_DATA_FILE.c_path)?,
        }
    } else {
        Traffic::None {
            fetch_data: hash_file(root, DATA_FILE.c_path)?,
        }
    };
    let fetch_info = FetchInfo {
        timestamp: meta.timestamp,
        log: hash_file(root, LOG_FILE.c_path)?,
        user: meta.user,
        traffic,
    };
    Ok(obj_store
        .add_object(webar_core::object::ObjectType::Record(ty), 1, &fetch_info)
        .map_err(|e| InnerError::Io(e.into()))?
        .id)
}

pub fn add_fetch_no_traffic<L, H, A, S, R>(
    root: BorrowedFd,
    obj_store: &object::WebsiteStore<H, A, S, R>,
    _: &blob::WebsiteStore,
    ty: R,
) -> Result<FetchId<L>, Error>
where
    L: Serialize + serde::de::DeserializeOwned,
    H: Serialize + Copy,
    A: Serialize,
    S: Serialize,
    R: Serialize,
{
    add_fetch(false, obj_store, root, ty).map_err(Error)
}
