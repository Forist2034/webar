use std::{
    ffi::CStr,
    fmt::Debug,
    io::{Read, Seek},
    marker::PhantomData,
};

use rustix::{
    fd::BorrowedFd,
    fs::{self, openat, OFlags},
};

use serde::de::DeserializeOwned;
use webar_core::{
    digest::{Digest, Sha256},
    fetch::{
        http::{
            internal::DigestField, FetchInfo, Metadata, Traffic, TrafficType, DATA_FILE,
            KEY_LOG_FILE, LOG_FILE, REQUEST_META_FILE, WIRESHARK_DATA_FILE,
        },
        FetchMeta, META_FILE,
    },
};

use crate::{blob, perm};

fn hash_std_file<F>(file: &mut std::fs::File) -> Result<DigestField<F>, std::io::Error> {
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
    std::io::copy(file, &mut writer)?;
    Ok(DigestField(
        Digest::Sha256(Sha256(sha2::Digest::finalize(writer.0).into())),
        PhantomData,
    ))
}

fn hash_file<F>(root: BorrowedFd, file: &CStr) -> Result<DigestField<F>, std::io::Error> {
    hash_std_file(&mut std::fs::File::from(fs::openat(
        root,
        file,
        OFlags::RDONLY,
        perm::IGNORE,
    )?))
}

fn read_file(root: BorrowedFd, file: &CStr) -> Result<Vec<u8>, std::io::Error> {
    let fd = fs::openat(root, file, OFlags::RDONLY, perm::IGNORE)?;
    let mut buf = Vec::new();
    std::fs::File::from(fd).read_to_end(&mut buf)?;
    Ok(buf)
}

#[derive(Debug, thiserror::Error)]
enum InnerError<T> {
    #[error("io error")]
    Io(
        #[source]
        #[from]
        std::io::Error,
    ),
    #[error("failed to decode metadata")]
    Decode(#[source] ciborium::de::Error<std::io::Error>),
    #[error("unexpected server name {0:?}")]
    ServerMismatch(String),
    #[error("unsupported version {0}")]
    UnsupportedVersion(u8),
    #[error("incorrect type {0:?}")]
    IncorrectType(T),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error<T>(InnerError<T>);

pub struct Fetch<I, L> {
    pub instance: I,
    pub info: FetchInfo<L>,
    pub data: std::fs::File,
}

fn read_fetch_inner<T, I, L>(
    server: &'static str,
    root: BorrowedFd,
    ty: T,
) -> Result<Fetch<I, L>, InnerError<T>>
where
    L: DeserializeOwned,
    I: DeserializeOwned,
    T: Debug + Eq + DeserializeOwned,
{
    let fetch_meta: FetchMeta<String, I, T, Metadata<L>> =
        ciborium::from_reader(read_file(root, META_FILE.c_path)?.as_slice())
            .map_err(InnerError::Decode)?;
    if fetch_meta.server != server {
        return Err(InnerError::ServerMismatch(fetch_meta.server));
    }
    if fetch_meta.ty != ty {
        return Err(InnerError::IncorrectType(fetch_meta.ty));
    }
    if fetch_meta.version != 1 {
        return Err(InnerError::UnsupportedVersion(fetch_meta.version));
    }
    let meta = fetch_meta.data;

    let mut data = std::fs::File::from(
        openat(root, DATA_FILE.c_path, OFlags::RDONLY, perm::IGNORE)
            .map_err(|e| InnerError::Io(e.into()))?,
    );
    let traffic = match meta.traffic {
        Some(TrafficType::Wireshark) => Traffic::Wireshark {
            key_log: hash_file(root, KEY_LOG_FILE.c_path)?,
            request_meta: hash_file(root, REQUEST_META_FILE.c_path)?,
            data: hash_file(root, WIRESHARK_DATA_FILE.c_path)?,
        },
        None => {
            let fetch_data = hash_std_file(&mut data)?;
            data.rewind()?;
            Traffic::None { fetch_data }
        }
    };
    Ok(Fetch {
        instance: fetch_meta.instance,
        info: FetchInfo {
            start_time: meta.start_time,
            end_time: meta.end_time,
            log: hash_file(root, LOG_FILE.c_path)?,
            user: meta.user,
            traffic,
        },
        data,
    })
}

pub fn read_fetch<T, I: DeserializeOwned, L: DeserializeOwned>(
    server: &'static str,
    root: BorrowedFd,
    ty: T,
    _: &blob::WebsiteStore,
) -> Result<Fetch<I, L>, Error<T>>
where
    T: Debug + Eq + DeserializeOwned,
{
    read_fetch_inner(server, root, ty).map_err(Error)
}
