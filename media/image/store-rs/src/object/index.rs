use std::{marker::PhantomData, path::Path};

use rusqlite::{Connection, OpenFlags};
use webar_core::{
    digest::{Digest, Sha256},
    object::ObjectId,
    FilePath,
};

pub struct ReadOnly;
pub struct ReadWrite;

pub const INDEX_FILE: FilePath = FilePath {
    path: "object_index.db",
    c_path: c"object_index.db",
};

pub const INDEX_PATH: FilePath = FilePath {
    path: "image/object_index.db",
    c_path: c"image/object_index.db",
};

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error(#[from] rusqlite::Error);

pub struct Index<S> {
    conn: Connection,
    _state: PhantomData<S>,
}
impl Index<ReadOnly> {
    pub fn open_ro(path: impl AsRef<Path>) -> Result<Self, Error> {
        Ok(Self {
            conn: Connection::open_with_flags(
                path,
                OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?,
            _state: PhantomData,
        })
    }
}
impl<S> Index<S> {
    pub fn url_exists(&self, url: &str) -> Result<bool, Error> {
        self.conn
            .prepare_cached("select * from url where url = ?")?
            .exists([url])
            .map_err(Error)
    }
    pub fn object_exists<T>(&self, id: &ObjectId<T>) -> Result<bool, Error> {
        self.conn
            .prepare_cached("select * from object_sha256 where sha256 = ?")?
            .exists([match &id.0 {
                Digest::Sha256(Sha256(d)) => d,
            }])
            .map_err(Error)
    }
}
impl Index<ReadWrite> {
    pub fn open_rw(path: impl AsRef<Path>) -> Result<Self, Error> {
        Ok(Self {
            conn: Connection::open_with_flags(
                path,
                OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?,
            _state: PhantomData,
        })
    }
    pub fn create(path: impl AsRef<Path>) -> Result<Self, Error> {
        let conn = Connection::open(path)?;
        conn.execute_batch(concat!(
            "create table if not exists object_sha256(sha256 blob primary key) strict;",
            "create table if not exists url(url text primary key) strict;"
        ))?;
        Ok(Self {
            conn,
            _state: PhantomData,
        })
    }
    pub fn insert_url(&self, url: &str) -> Result<(), Error> {
        self.conn
            .prepare_cached("insert or ignore into url (url) values (?1)")?
            .execute([url])?;
        Ok(())
    }
    pub fn insert_object<T>(&self, id: &ObjectId<T>) -> Result<(), Error> {
        self.conn
            .prepare_cached("insert or ignore into object_sha256 (sha256) values (?1)")?
            .execute([match &id.0 {
                Digest::Sha256(Sha256(d)) => d,
            }])?;
        Ok(())
    }
}
