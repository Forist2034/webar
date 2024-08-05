{-# LANGUAGE LambdaCase #-}

module Webar.Store.FileSystem
  ( dropFileName,
    buildPath,
    createDirectoryAt,
    createDirectoryIfMissingAt,
    createFileAt,
    linkAt,
    linkFileAt,
    setFdModeAt,
    createReadOnlyDirAt,
  )
where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Foreign.C
import Foreign.Ptr (plusPtr)
import System.IO.Error
import qualified System.Posix.ByteString as P
import System.Posix.ByteString.FilePath
import System.Posix.Types
import Webar.Bytes (ByteArrayAccess)
import qualified Webar.Bytes as Bytes

foreign import ccall unsafe "mkdirat"
  c_mkdirat :: CInt -> CString -> CMode -> IO CInt

foreign import ccall unsafe "linkat"
  c_linkat :: CInt -> CString -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe "fchmodat"
  c_fchmodat :: CInt -> CString -> CMode -> CInt -> IO CInt

dropFileName :: ByteString -> ByteString
dropFileName p = case BS.unsnoc p of
  Just (p1, 47) -> BS.dropWhileEnd (/= 47) p1
  _ -> BS.dropWhileEnd (/= 47) p

buildPath :: BSB.Builder -> RawFilePath
buildPath = BS.toStrict . BSB.toLazyByteString

createFileAt :: (ByteArrayAccess b) => Fd -> RawFilePath -> b -> IO ()
createFileAt atFd p buf =
  Bytes.withByteArray
    buf
    ( \ptr ->
        bracket
          ( P.openFdAt
              (Just atFd)
              p
              P.WriteOnly
              P.defaultFileFlags {P.exclusive = True, P.creat = Just 0o444}
          )
          P.closeFd
          (write ptr (fromIntegral (Bytes.length buf)))
    )
  where
    write _ 0 _ = pure ()
    write ptr cnt fd =
      P.fdWriteBuf fd ptr cnt >>= \l ->
        write (ptr `plusPtr` fromIntegral l) (cnt - l) fd

createDirectoryAt :: Fd -> RawFilePath -> CMode -> IO ()
createDirectoryAt (Fd fd) name m =
  withFilePath name (\s -> throwErrnoIfMinus1_ "createDirectoryAt" (c_mkdirat fd s m))

createDirectoryIfMissingAt :: Fd -> RawFilePath -> CMode -> IO Bool
createDirectoryIfMissingAt fd name m =
  tryIOError (createDirectoryAt fd name m) >>= \case
    Right _ -> pure True
    Left e
      | isAlreadyExistsError e -> pure False
      | isDoesNotExistError e -> do
          go (dropFileName name)
          createDirectoryAt fd name m
          pure True
      | otherwise -> ioError e
  where
    go path =
      tryIOError (createDirectoryAt fd path 0o777) >>= \case
        Right _ -> pure ()
        Left e
          | isAlreadyExistsError e -> pure ()
          | isDoesNotExistError e ->
              go (dropFileName path) >> createDirectoryAt fd path 0o777
          | otherwise -> ioError e

linkAt :: Fd -> RawFilePath -> Fd -> RawFilePath -> IO ()
linkAt (Fd oldFd) oldPath (Fd newFd) newPath =
  withFilePath
    oldPath
    ( \os ->
        withFilePath
          newPath
          ( \ns ->
              throwErrnoIfMinus1_ "linkAt" (c_linkat oldFd os newFd ns 0)
          )
    )

-- | linkAt that use same path
linkFileAt :: Fd -> Fd -> RawFilePath -> IO ()
linkFileAt (Fd oldFd) (Fd newFd) path =
  withFilePath
    path
    ( \p ->
        throwErrnoIfMinus1_ "linkFileAt" (c_linkat oldFd p newFd p 0)
    )

setFdModeAt :: Fd -> RawFilePath -> CMode -> IO ()
setFdModeAt (Fd fd) path mode =
  withFilePath
    path
    ( \p ->
        throwErrnoIfMinus1_ "setFdModeAt" (c_fchmodat fd p mode 0)
    )

createReadOnlyDirAt :: Fd -> RawFilePath -> IO () -> IO ()
createReadOnlyDirAt fd path act = do
  createDirectoryIfMissingAt fd path 0o755 >>= \case
    True -> act >> setFdModeAt fd path 0o555
    False -> pure ()