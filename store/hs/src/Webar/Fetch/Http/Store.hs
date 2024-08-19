{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Fetch.Http.Store
  ( addWiresharkFetch,
    addFetchNoTraffic,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import System.FilePath ((</>))
import System.IO (hClose)
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import Webar.Data.Cbor (ToCbor)
import Webar.Data.Json (FromJSON)
import qualified Webar.Data.Json as Json
import Webar.Data.Json.TH
import Webar.Digest (hashHandle)
import Webar.Fetch.Http
import Webar.Fetch.Http.Internal
import Webar.Object
import qualified Webar.Store.Blob.WithShared as BS
import qualified Webar.Store.Object.Website as OS
import Webar.Types (Timestamp)

data FetchMeta l = FetchMeta
  { fmTimestamp :: Timestamp,
    fmUser :: Maybe l
  }
  deriving (Show)

deriveProdFromJSON
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchMeta

hashFile :: P.Fd -> P.RawFilePath -> IO (DigestField t)
hashFile dirFd p =
  bracket
    (P.openFdAt (Just dirFd) p P.ReadOnly P.defaultFileFlags >>= P.fdToHandle)
    hClose
    (fmap DigestField . hashHandle)

fetchInfo :: (FromJSON l) => FilePath -> Bool -> IO (FetchInfo l)
fetchInfo p isWireshark = do
  meta <- Json.decodeStrictBsThrow <$> BS.readFile (p </> "meta.json")
  bracket
    (P.FilePath.openFd p P.FilePath.ReadOnly P.FilePath.defaultFileFlags)
    P.FilePath.closeFd
    ( \fd -> do
        logFile <- hashFile fd "log.bin"
        traffic <-
          if isWireshark
            then do
              keyLog <- hashFile fd "sslkeylog.keys"
              reqMeta <- hashFile fd "request_meta.tar"
              reqData <- hashFile fd "traffic.pcapng"
              pure
                TWireshark
                  { twKeyLog = keyLog,
                    twRequestMeta = reqMeta,
                    twData = reqData
                  }
            else TNone <$> hashFile fd "data.tar"
        pure
          ( FetchInfo
              { tiTimestamp = fmTimestamp meta,
                tiLog = logFile,
                tiUser = fmUser meta,
                tiTraffic = traffic
              }
          )
    )

addFetch ::
  (FromJSON l, ToCbor h, ToCbor st, ToCbor rt, ToCbor l) =>
  OS.ObjectStore h a st rt ->
  Bool ->
  rt ->
  FilePath ->
  IO (ObjectId (FetchInfo l))
addFetch objStore isWireshark ty fetch =
  fetchInfo fetch isWireshark >>= \info ->
    OS.objectId <$> OS.addObject objStore (OtRecord ty) 1 info

addWiresharkFetch ::
  (FromJSON l, ToCbor h, ToCbor st, ToCbor rt, ToCbor l) =>
  OS.ObjectStore h a st rt ->
  BS.BlobStore ->
  rt ->
  FilePath ->
  IO (FetchId l)
addWiresharkFetch objStore _ = addFetch objStore True

addFetchNoTraffic ::
  (FromJSON l, ToCbor h, ToCbor st, ToCbor rt, ToCbor l) =>
  OS.ObjectStore h a st rt ->
  BS.BlobStore ->
  rt ->
  FilePath ->
  IO (ObjectId (FetchInfo l))
addFetchNoTraffic objStore _ = addFetch objStore False
