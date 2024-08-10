use std::{marker::PhantomData, path::Path};

use rusqlite::{Connection, OpenFlags};

use webar_core::{
    blob::BlobId,
    digest::{Digest, Sha256},
};

pub struct ReadOnly;
pub struct ReadWrite;

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
            conn: Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_ONLY)?,
            _state: PhantomData,
        })
    }
}
impl<S> Index<S> {
    pub fn exists<T>(&self, id: &BlobId<T>) -> Result<bool, Error> {
        self.conn
            .prepare_cached("select exists(*) from sha256 where sha256 = ?")?
            .exists((
                1,
                match &id.0 {
                    Digest::Sha256(Sha256(d)) => d,
                },
            ))
            .map_err(Error)
    }
}
impl Index<ReadWrite> {
    pub fn open_rw(path: impl AsRef<Path>) -> Result<Self, Error> {
        Ok(Self {
            conn: Connection::open(path)?,
            _state: PhantomData,
        })
    }
    pub fn create(path: impl AsRef<Path>) -> Result<Self, Error> {
        let conn = Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_CREATE)?;
        conn.execute(
            "create table if not exists sha256 (sha256 blob primary key) strict",
            (),
        )?;
        Ok(Self {
            conn,
            _state: PhantomData,
        })
    }
    pub fn insert<T>(&self, id: &BlobId<T>) -> Result<(), Error> {
        self.conn
            .prepare_cached("insert or ignore into sha256 (sha256) values (?)")?
            .insert((
                1,
                match &id.0 {
                    Digest::Sha256(Sha256(d)) => d,
                },
            ))?;
        Ok(())
    }
}
