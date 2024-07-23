{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Fetch.Http.Store (addWiresharkFetch) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Proxy (Proxy)
import System.IO (hClose)
import System.OsPath.Posix
import qualified System.OsString.Posix as OSStr
import qualified System.Posix.IO.PosixString as P
import Webar.Data.Cbor (ToCbor)
import qualified Webar.Data.Cbor as Cbor
import Webar.Data.Json (FromJSON)
import qualified Webar.Data.Json as Json
import Webar.Data.Json.TH (ProductOptions (..), camelTo2, deriveProdFromJSON)
import Webar.Digest
import Webar.Fetch.Http
import Webar.Store.FileSystem
import Webar.Types

data FetchMeta l = FetchMeta
  { fmTimestamp :: Timestamp,
    fmUser :: Maybe l
  }
  deriving (Show)

deriveProdFromJSON
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchMeta

sha256File :: PosixPath -> IO Sha256
sha256File path =
  bracket
    (P.openFd path P.ReadOnly P.defaultFileFlags >>= P.fdToHandle)
    hClose
    sha256Handle

fetchInfo :: (FromJSON l) => PosixPath -> IO (FetchInfo l)
fetchInfo p = do
  meta <-
    decodeFS (p </> [pstr|meta.json|]) >>= \path ->
      Json.decodeStrictBsThrow <$> BS.readFile path
  keyLog <- sha256File (p </> [pstr|sslkeylog.keys|])
  logFile <- sha256File (p </> [pstr|log.bin|])
  reqMeta <- sha256File (p </> [pstr|request_meta.tar|])
  reqData <- sha256File (p </> [pstr|traffic.pcapng|])
  pure
    ( FetchInfo
        { tiTimestamp = fmTimestamp meta,
          tiLog = DSha256 logFile,
          tiUser = fmUser meta,
          tiKeyLog = Just (DSha256 keyLog),
          tiTraffic =
            TWireshark
              { twKeyLog = DSha256 keyLog,
                twRequestMeta = DSha256 reqMeta,
                twData = DSha256 reqData
              }
        }
    )

addWiresharkFetch ::
  forall l.
  (FromJSON l, ToCbor l) =>
  -- | store root
  PosixPath ->
  Proxy l ->
  -- | fetch path
  PosixPath ->
  IO FetchId
addWiresharkFetch root _ fetch = do
  info <- Cbor.encodeStrictBs <$> fetchInfo @l fetch
  let sha256 = sha256Hash info
  let path =
        root
          </> OSStr.fromBytestring
            ( BS.toStrict
                ( BSB.toLazyByteString
                    ( "http/fetch.sha256/"
                        <> (let SubDir sub _ _ _ = sha256SubDir sha256 in BSB.word8HexFixed sub)
                        <> "/"
                        <> BSB.stringUtf8 (sha256ToString sha256)
                    )
                )
            )
  createObjectDir
    path
    ( do
        createFile (path </> [pstr|info.bin|]) info
        link path [pstr|log.bin|]
        link path [pstr|sslkeylog.keys|]
        link path [pstr|request_meta.tar|]
        link path [pstr|traffic.pcapng|]
    )
  pure (FetchId (DSha256 sha256))
  where
    link path name = createLink (fetch </> name) (path </> name)
