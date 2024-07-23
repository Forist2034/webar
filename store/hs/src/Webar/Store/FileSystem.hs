{-# LANGUAGE LambdaCase #-}

module Webar.Store.FileSystem
  ( createFile,
    P.createLink,
    createObjectDir,
  )
where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Directory.OsPath as D
import System.IO.Error
import System.OsPath.Posix
import System.OsString.Internal.Types (OsString (OsString))
import qualified System.Posix.PosixString as P

createFile :: PosixPath -> ByteString -> IO ()
createFile p content =
  bracket
    ( P.openFd
        p
        P.WriteOnly
        P.defaultFileFlags {P.exclusive = True, P.creat = Just 0o444}
    )
    P.closeFd
    (write content)
  where
    write bs fd
      | BS.null bs = pure ()
      | otherwise =
          P.fdWrite fd bs >>= \l ->
            write (BS.drop (fromIntegral l) bs) fd

createObjectDir :: PosixPath -> IO () -> IO ()
createObjectDir path act = do
  D.createDirectoryIfMissing True (OsString (dropFileName path))
  tryIOError (P.createDirectory path 0o755) >>= \case
    Right _ -> act >> P.setFileMode path 0o555
    Left e
      | isAlreadyExistsError e -> pure ()
      | otherwise -> ioError e