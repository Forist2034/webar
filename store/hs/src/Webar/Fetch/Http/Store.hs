{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Fetch.Http.Store
  ( Fetch (..),
    withFetch,
  )
where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Word (Word8)
import System.FilePath ((</>))
import System.IO
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import Webar.Data.Json (FromJSON)
import qualified Webar.Data.Json as Json
import Webar.Digest (hashHandle)
import Webar.Fetch
import Webar.Fetch.Http
import Webar.Fetch.Http.Internal
import qualified Webar.Store.Blob.WithShared as BS

data Fetch i l = Fetch
  { fInstance :: i,
    fInfo :: FetchInfo l,
    fData :: Handle
  }

hashFile :: P.Fd -> P.RawFilePath -> IO (DigestField t)
hashFile dirFd p =
  bracket
    (P.openFdAt (Just dirFd) p P.ReadOnly P.defaultFileFlags >>= P.fdToHandle)
    hClose
    (fmap DigestField . hashHandle)

data MetaException t
  = ServerMismatch Text
  | UnsupportedVersion Word8
  | IncorrectType t
  deriving (Show)

instance (Show t, Typeable t) => Exception (MetaException t)

readFetch ::
  forall i t l.
  (FromJSON i, FromJSON t, Show t, Eq t, Typeable t, FromJSON l) =>
  Text ->
  t ->
  FilePath ->
  IO (Fetch i l)
readFetch server ty p = do
  fetchMeta <- Json.decodeStrictBsThrow <$> BS.readFile (p </> "meta.json")
  unless (fmServer fetchMeta == server) (throwIO (ServerMismatch @t (fmServer fetchMeta)))
  unless (fmType fetchMeta == ty) (throwIO (IncorrectType (fmType fetchMeta)))
  unless (fmVersion fetchMeta == 1) (throwIO (UnsupportedVersion @t (fmVersion fetchMeta)))
  let meta = fmData fetchMeta
  bracket
    (P.FilePath.openFd p P.FilePath.ReadOnly P.FilePath.defaultFileFlags)
    P.FilePath.closeFd
    ( \fd -> do
        logFile <- hashFile fd "log.bin"
        dataFile <- P.openFdAt (Just fd) "data.tar" P.ReadOnly P.defaultFileFlags >>= P.fdToHandle
        traffic <- case metaTraffic meta of
          Just TtWireshark -> do
            keyLog <- hashFile fd "sslkeylog.keys"
            reqMeta <- hashFile fd "request_meta.tar"
            reqData <- hashFile fd "traffic.pcapng"
            pure
              TWireshark
                { twKeyLog = keyLog,
                  twRequestMeta = reqMeta,
                  twData = reqData
                }
          Nothing -> do
            h <- hashHandle dataFile
            hSeek dataFile AbsoluteSeek 0
            pure (TNone (DigestField h))
        pure
          Fetch
            { fInstance = fmInstance fetchMeta,
              fInfo =
                FetchInfo
                  { tiStartTime = metaStartTime meta,
                    tiEndTime = metaEndTime meta,
                    tiUser = metaUser meta,
                    tiTraffic = traffic,
                    tiLog = logFile
                  },
              fData = dataFile
            }
    )

withFetch ::
  forall i t l c.
  (FromJSON i, Show t, Eq t, Typeable t, FromJSON t, FromJSON l) =>
  BS.BlobStore ->
  Text ->
  t ->
  FilePath ->
  (Fetch i l -> IO c) ->
  IO c
withFetch _ server ty p =
  bracket (readFetch server ty p) (hClose . fData)
