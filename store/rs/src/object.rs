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
    digest::{Digest, Sha256},
    object::{self, ObjectId, ObjectInfo, ObjectType, Server},
};
use webar_data::ser::Serialize;

use crate::{perm, utils};

fn to_path(digest: &Digest) -> PathBuf {
    let mut ret = PathBuf::with_capacity(6 + 1 + 4 + 1 + 64 + 1 + 8);
    match digest {
        Digest::Sha256(Sha256(d)) => {
            ret.push("sha256");
            {
                let mut sub = [0; 4];
                const_hex::encode_to_slice(&d[0..2], &mut sub).unwrap();
                ret.push(unsafe { from_utf8_unchecked(&sub) });
            }
            {
                let mut hex = [0; 64];
                const_hex::encode_to_slice(d, &mut hex).unwrap();
                ret.push(unsafe { from_utf8_unchecked(&hex) });
            }
        }
    }
    ret.push("info.bin");
    ret
}

pub struct ObjectHandle<T> {
    pub id: ObjectId<T>,
    file: PathBuf,
}

pub struct BaseStore(OwnedFd);
impl BaseStore {
    pub fn open(path: &Path) -> Result<Self, Errno> {
        fs::open(path, OFlags::PATH | OFlags::DIRECTORY, perm::IGNORE).map(Self)
    }
    pub fn open_at(dir: BorrowedFd, path: impl Arg) -> Result<Self, Errno> {
        fs::openat(dir, path, OFlags::PATH | OFlags::DIRECTORY, perm::IGNORE).map(Self)
    }
    pub fn add_object<N, Host, Archive, Snapshot, Record, Data>(
        &self,
        server: &Server<N>,
        info: &ObjectInfo<Host, Archive, Snapshot, Record>,
        data: &Data,
    ) -> Result<ObjectHandle<Data>, Errno>
    where
        N: AsRef<str>,
        Host: Serialize,
        Archive: Serialize,
        Snapshot: Serialize,
        Record: Serialize,
        Data: Serialize,
    {
        let bytes = object::encode_object(server, info, data);
        let digest = Digest::digest(&bytes);
        let path = to_path(&digest);
        utils::create_ro_dir_at(self.0.as_fd(), path.parent().unwrap(), || {
            utils::create_file(self.0.as_fd(), &path, &bytes)
        })?;
        Ok(ObjectHandle {
            id: ObjectId(digest, PhantomData),
            file: path,
        })
    }
    fn link_path(&self, old: &Self, path: &Path) -> Result<(), Errno> {
        utils::create_ro_dir_at(self.0.as_fd(), path.parent().unwrap(), || {
            fs::linkat(old.0.as_fd(), path, self.0.as_fd(), path, AtFlags::empty())
        })
    }
    pub fn link_handle<T>(&self, old: &Self, handle: &ObjectHandle<T>) -> Result<(), Errno> {
        self.link_path(old, &handle.file)
    }
    pub fn link<T>(&self, old: &Self, id: &ObjectId<T>) -> Result<(), Errno> {
        self.link_path(old, &to_path(&id.0))
    }
}

pub struct WebsiteStore<'a, Host, Archive, Snapshot, Record> {
    server: Server<&'static str>,
    host: Host,
    pub base: &'a BaseStore,
    upper: BaseStore,
    _phantom: PhantomData<fn(Archive, Snapshot, Record)>,
}
impl<'a, Host, Archive, Snapshot, Record> WebsiteStore<'a, Host, Archive, Snapshot, Record>
where
    Host: Serialize + Copy,
    Archive: Serialize,
    Snapshot: Serialize,
    Record: Serialize,
{
    pub fn open_at(
        server: Server<&'static str>,
        host: Host,
        base: &'a BaseStore,
        dir: BorrowedFd,
        path: impl Arg,
    ) -> Result<Self, Errno> {
        Ok(Self {
            server,
            host,
            base,
            upper: BaseStore::open_at(dir, path)?,
            _phantom: PhantomData,
        })
    }
    pub fn add_object<T: Serialize>(
        &self,
        ty: ObjectType<Archive, Snapshot, Record>,
        version: u8,
        data: &T,
    ) -> Result<ObjectHandle<T>, Errno> {
        let ret = self.base.add_object(
            &self.server,
            &ObjectInfo {
                host: self.host,
                ty,
                version,
            },
            data,
        )?;
        self.upper.link_handle(self.base, &ret)?;
        Ok(ret)
    }
}
