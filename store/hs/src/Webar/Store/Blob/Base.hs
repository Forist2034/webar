{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Store.Blob.Base
  ( BlobHandle (blobId),
    BlobStore,
    openByFilePath,
    addBlob,
    linkHandle,
    linkBlob,
    close,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Proxy (Proxy (Proxy))
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import System.Posix.Types (Fd)
import Webar.Blob
import Webar.Digest
import Webar.Store.FileSystem

data Path = Path !ByteString !ByteString

toPath :: Bool -> Digest -> Path
toPath img digest =
  let dataP =
        buildPath
          ( (if img then "media/" else "data/")
              <> ( case digest of
                     DSha256 sha256 ->
                       let SubDir s0 s1 = sha256SubDir sha256
                        in "sha256/"
                             <> BSB.word8HexFixed s0
                             <> BSB.word8HexFixed s1
                             <> "/"
                             <> BSB.string7 (sha256ToString sha256)
                 )
              <> "/data"
          )
   in Path (BS.dropEnd 5 dataP) dataP

data BlobHandle t = BlobHandle {blobId :: !(BlobId t), blobPath :: !Path}

newtype BlobStore = BlobStore Fd

openByFilePath :: FilePath -> IO BlobStore
openByFilePath fp =
  BlobStore
    <$> P.FilePath.openFd
      fp
      P.FilePath.ReadOnly
      P.FilePath.defaultFileFlags {P.FilePath.directory = True}

addBlob :: forall t. (BlobData t) => BlobStore -> t -> IO (BlobHandle t)
addBlob (BlobStore fd) content =
  let digest = hashBytes content
      path@(Path dir file) = toPath (isImage @t Proxy) digest
   in BlobHandle (BlobId digest) path
        <$ createReadOnlyDirAt fd dir (createFileAt fd file content)

linkPath :: BlobStore -> BlobStore -> Path -> IO ()
linkPath (BlobStore newFd) (BlobStore oldFd) (Path dir file) =
  createReadOnlyDirAt newFd dir (linkFileAt oldFd newFd file)

linkHandle :: BlobStore -> BlobStore -> BlobHandle t -> IO ()
linkHandle new old h = linkPath new old (blobPath h)

linkBlob ::
  forall t.
  (BlobData t) =>
  BlobStore ->
  -- | old store
  BlobStore ->
  BlobId t ->
  IO ()
linkBlob new old (BlobId d) = linkPath new old (toPath (isImage @t Proxy) d)

close :: BlobStore -> IO ()
close (BlobStore fd) = P.closeFd fd