{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Webar.Store.File.DedupStore
  ( DedupStore,
    openOrCreate,
    addFile,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified System.Directory.OsPath as D
import System.IO.Error
import System.OsPath.Posix
import System.OsString.Internal.Types (OsString (OsString))
import qualified System.OsString.Posix as OSStr
import qualified System.Posix.PosixString as P
import Webar.Digest
import Webar.Store.FileSystem
import Webar.Types

newtype DedupStore = DedupStore PosixPath

openOrCreate :: PosixPath -> IO DedupStore
openOrCreate p = DedupStore p <$ D.createDirectoryIfMissing True (OsString p)

addFile :: DedupStore -> Digest -> BS.ByteString -> PosixPath -> IO ()
addFile (DedupStore root) (DSha256 sha256) content dest = do
  let path =
        root
          </> OSStr.fromBytestring
            ( BS.toStrict
                ( BSB.toLazyByteString
                    ( "sha256/"
                        <> ( let SubDir sub0 sub1 _ _ = sha256SubDir sha256
                              in BSB.word8HexFixed sub0 <> BSB.word8HexFixed sub1
                           )
                        <> "/"
                        <> BSB.stringUtf8 (sha256ToString sha256)
                    )
                )
            )
  D.createDirectoryIfMissing True (OsString (dropFileName path))
  tryIOError (createFile path content) >>= \case
    Right _ -> pure ()
    Left e
      | isAlreadyExistsError e -> pure ()
      | otherwise -> ioError e
  P.createLink path dest