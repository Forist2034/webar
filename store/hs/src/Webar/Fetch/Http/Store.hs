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
import System.FilePath ((</>))
import System.IO
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import Webar.Data.Cbor (FromCbor)
import qualified Webar.Data.Cbor as Cbor
import Webar.Digest (hashHandle)
import Webar.Fetch
import Webar.Fetch.Http
import Webar.Fetch.Http.Internal
import qualified Webar.Store.Blob.WithShared as BS
import Webar.Types (Server, Version (Version))

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
  = ServerMismatch Server
  | UnsupportedVersion Version
  | IncorrectType t
  deriving (Show)

instance (Show t, Typeable t) => Exception (MetaException t)

readFetch ::
  forall i t l.
  (FromCbor i, FromCbor t, Show t, Eq t, Typeable t, FromCbor l) =>
  Server ->
  t ->
  FilePath ->
  IO (Fetch i l)
readFetch server ty p = do
  fetchMeta <- Cbor.decodeStrictBsThrow <$> BS.readFile (p </> "meta.bin")
  unless (fmServer fetchMeta == server) (throwIO (ServerMismatch @t (fmServer fetchMeta)))
  unless (fmType fetchMeta == ty) (throwIO (IncorrectType (fmType fetchMeta)))
  unless (fmVersion fetchMeta == Version 1 0) (throwIO (UnsupportedVersion @t (fmVersion fetchMeta)))
  let meta = fmData fetchMeta
  bracket
    (P.FilePath.openFd p P.FilePath.ReadOnly P.FilePath.defaultFileFlags)
    P.FilePath.closeFd
    ( \fd -> do
        logFile <- hashFile fd "tracing.log.bin"
        dataFile <- P.openFdAt (Just fd) "data.tar" P.ReadOnly P.defaultFileFlags >>= P.fdToHandle
        traffic <- case metaTraffic meta of
          Just TtWireshark -> do
            keyLog <- hashFile fd "sslkeylog.keys"
            dumpcapLog <- hashFile fd "dumpcap.log"
            reqMeta <- hashFile fd "request_meta.tar"
            reqData <- hashFile fd "traffic.pcapng"
            pure
              TWireshark
                { twKeyLog = keyLog,
                  twDumpcapLog = dumpcapLog,
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
                    tiTracingLog = logFile
                  },
              fData = dataFile
            }
    )

withFetch ::
  forall i t l c.
  (FromCbor i, Show t, Eq t, Typeable t, FromCbor t, FromCbor l) =>
  BS.BlobStore ->
  Server ->
  t ->
  FilePath ->
  (Fetch i l -> IO c) ->
  IO c
withFetch _ server ty p =
  bracket (readFetch server ty p) (hClose . fData)
