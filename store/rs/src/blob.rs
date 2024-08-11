use std::{
    marker::PhantomData,
    path::{Path, PathBuf},
    str::from_utf8_unchecked,
};

use rustix::{
    fd::{AsFd, BorrowedFd, OwnedFd},
    fs::{self, AtFlags, OFlags},
    io::Errno,
    path::Arg,
};
use webar_core::{
    blob::{is_image, BlobData, BlobId},
    digest::{Digest, Sha256},
};

use crate::{perm, utils};

pub struct BlobHandle<T> {
    pub id: BlobId<T>,
    file: PathBuf,
}

fn to_path(is_media: bool, digest: &Digest) -> PathBuf {
    let mut ret = PathBuf::new();
    ret.push(if is_media { "media" } else { "data" });
    match digest {
        Digest::Sha256(Sha256(d)) => {
            ret.push("sha256");
            {
                let mut subdir = [0; 4];
                const_hex::encode_to_slice(&d[0..2], &mut subdir).unwrap();
                ret.push(unsafe { from_utf8_unchecked(&subdir) });
            }
            {
                let mut hex = [0; 64];
                const_hex::encode_to_slice(d, &mut hex).unwrap();
                ret.push(unsafe { from_utf8_unchecked(&hex) });
            }
        }
    }
    ret.push("data");
    ret
}

pub struct BaseStore(OwnedFd);
impl BaseStore {
    pub fn open(path: impl Arg) -> Result<Self, Errno> {
        fs::open(path, OFlags::PATH | OFlags::DIRECTORY, perm::IGNORE).map(Self)
    }
    pub fn open_at(dirfd: BorrowedFd, path: impl Arg) -> Result<Self, Errno> {
        fs::openat(dirfd, path, OFlags::PATH | OFlags::DIRECTORY, perm::IGNORE).map(Self)
    }
    pub fn add_blob<B: BlobData>(&self, data: &B) -> Result<BlobHandle<B>, Errno> {
        let digest = Digest::digest(data.as_ref());
        let path = to_path(is_image::<B>(), &digest);
        utils::create_ro_dir_at(self.0.as_fd(), path.parent().unwrap(), || {
            utils::create_file(self.0.as_fd(), &path, data.as_ref())
        })?;
        Ok(BlobHandle {
            id: BlobId(digest, PhantomData),
            file: path,
        })
    }
    fn link_path(&self, old: &Self, path: &Path) -> Result<(), Errno> {
        utils::create_ro_dir_at(self.0.as_fd(), path.parent().unwrap(), || {
            fs::linkat(old.0.as_fd(), path, self.0.as_fd(), path, AtFlags::empty())
        })
    }
    pub fn link_handle<T>(&self, old: &Self, handle: &BlobHandle<T>) -> Result<(), Errno> {
        self.link_path(old, &handle.file)
    }
    pub fn link<B: BlobData>(&self, old: &Self, id: &BlobId<B>) -> Result<(), Errno> {
        let path = to_path(is_image::<B>(), &id.0);
        self.link_path(old, &path)?;
        Ok(())
    }
}

pub struct WebsiteStore<'a> {
    pub base: &'a BaseStore,
    upper: BaseStore,
}
impl<'a> WebsiteStore<'a> {
    pub fn open_at(base: &'a BaseStore, dirfd: BorrowedFd, path: impl Arg) -> Result<Self, Errno> {
        Ok(Self {
            base,
            upper: BaseStore::open_at(dirfd, path)?,
        })
    }
    pub fn add_blob<B: BlobData>(&self, data: &B) -> Result<BlobHandle<B>, Errno> {
        let ret = self.base.add_blob(data)?;
        self.upper.link_handle(&self.base, &ret)?;
        Ok(ret)
    }
    pub fn link<B: BlobData>(&self, old: &BaseStore, id: &BlobId<B>) -> Result<(), Errno> {
        let path = to_path(is_image::<B>(), &id.0);
        self.base.link_path(old, &path)?;
        // base store may already have blob, so always link to base store
        self.upper.link_path(self.base, &path)
    }
}
