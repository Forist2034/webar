{-# LANGUAGE OverloadedStrings #-}

module Webar.Store.Data.Base
  ( DataHandle (dataId),
    DataStore,
    openByFilePath,
    addByteString,
    addFileByRawPathAt,
    linkHandle,
    linkData,
    close,
  )
where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import System.IO
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import System.Posix.Types (Fd)
import Webar.Digest
import Webar.Object
import Webar.Store.FileSystem

data Path = Path !ByteString !ByteString

toPath :: Digest -> Path
toPath digest =
  let dataP =
        buildPath
          ( ( case digest of
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

data DataHandle = DataHandle {dataId :: !DataId, dataPath :: !Path}

newtype DataStore = DataStore Fd

openByFilePath :: FilePath -> IO DataStore
openByFilePath fp =
  DataStore
    <$> P.FilePath.openFd
      fp
      P.FilePath.ReadOnly
      P.FilePath.defaultFileFlags {P.FilePath.directory = True}

addByteString :: DataStore -> ByteString -> IO DataHandle
addByteString (DataStore fd) content =
  let digest = hashBytes content
      path@(Path dir file) = toPath digest
   in DataHandle (DataId digest) path
        <$ createReadOnlyDirAt fd dir (createFileAt fd file content)

-- | add file by path, require file to be immutable
addFileByRawPathAt :: DataStore -> Fd -> ByteString -> IO DataHandle
addFileByRawPathAt (DataStore storeFd) dirFd fp =
  bracket
    ( P.openFdAt (Just dirFd) fp P.ReadOnly P.defaultFileFlags
        >>= \fd -> P.fdToHandle fd
    )
    hClose
    ( \h -> do
        digest <- hashHandle h
        let path@(Path dir file) = toPath digest
        createReadOnlyDirAt storeFd dir (linkAt dirFd fp storeFd file)
        pure (DataHandle (DataId digest) path)
    )

linkPath :: DataStore -> DataStore -> Path -> IO ()
linkPath (DataStore newFd) (DataStore oldFd) (Path dir file) =
  createReadOnlyDirAt newFd dir (linkFileAt oldFd newFd file)

linkHandle :: DataStore -> DataStore -> DataHandle -> IO ()
linkHandle new old h = linkPath new old (dataPath h)

linkData ::
  DataStore ->
  -- | old store
  DataStore ->
  DataId ->
  IO ()
linkData new old (DataId d) = linkPath new old (toPath d)

close :: DataStore -> IO ()
close (DataStore fd) = P.closeFd fd