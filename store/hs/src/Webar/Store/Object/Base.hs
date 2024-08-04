{-# LANGUAGE OverloadedStrings #-}

module Webar.Store.Object.Base
  ( ObjectHandle (objectId),
    ObjectStore,
    openByFilePath,
    addObject,
    linkHandle,
    linkObject,
    close,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import System.Posix.Types
import Webar.Data.Cbor (ToCbor)
import Webar.Digest
import Webar.Object
import Webar.Store.FileSystem

data Path = Path !ByteString !ByteString

toPath :: Digest -> Path
toPath d =
  let dataP =
        buildPath
          ( ( case d of
                DSha256 sha256 ->
                  let SubDir s0 s1 = sha256SubDir sha256
                   in "sha256/"
                        <> BSB.word8HexFixed s0
                        <> BSB.word8HexFixed s1
                        <> "/"
                        <> BSB.string7 (sha256ToString sha256)
            )
              <> "/info.bin"
          )
   in Path (BS.dropEnd 9 dataP) dataP

data ObjectHandle t = ObjectHandle
  { objectId :: !(ObjectId t),
    objectPath :: !Path
  }

newtype ObjectStore = ObjectStore Fd

openByFilePath :: FilePath -> IO ObjectStore
openByFilePath fp =
  ObjectStore
    <$> P.FilePath.openFd
      fp
      P.FilePath.ReadOnly
      P.FilePath.defaultFileFlags {P.FilePath.directory = True}

addObject ::
  (ToCbor h, ToCbor st, ToCbor rt, ToCbor o) =>
  ObjectStore ->
  Server ->
  ObjectInfo h a st rt ->
  o ->
  IO (ObjectHandle o)
addObject (ObjectStore fd) server objInfo cont =
  let bytes = encodeObject server objInfo cont
      digest = hashBytes bytes
      path@(Path dir file) = toPath digest
   in ObjectHandle (ObjectId digest) path
        <$ createReadOnlyDirAt fd dir (createFileAt fd file bytes)

linkPath :: ObjectStore -> ObjectStore -> Path -> IO ()
linkPath (ObjectStore newFd) (ObjectStore oldFd) (Path dir file) =
  createReadOnlyDirAt newFd dir (linkFileAt oldFd newFd file)

linkHandle :: ObjectStore -> ObjectStore -> ObjectHandle o -> IO ()
linkHandle new old h = linkPath new old (objectPath h)

linkObject :: ObjectStore -> ObjectStore -> ObjectId o -> IO ()
linkObject new old (ObjectId d) = linkPath new old (toPath d)

close :: ObjectStore -> IO ()
close (ObjectStore fd) = P.closeFd fd