{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Fetch.Http.Store (addWiresharkFetch) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import System.FilePath ((</>))
import System.IO (hClose)
import qualified System.Posix as P.FilePath
import qualified System.Posix.ByteString as P
import Webar.Data.Cbor (ToCbor)
import Webar.Data.Json (FromJSON)
import qualified Webar.Data.Json as Json
import Webar.Data.Json.TH (ProductOptions (..), camelTo2, deriveProdFromJSON)
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
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchMeta

hashFile :: P.Fd -> P.RawFilePath -> IO (DigestField t)
hashFile dirFd p =
  bracket
    (P.openFdAt (Just dirFd) p P.ReadOnly P.defaultFileFlags >>= P.fdToHandle)
    hClose
    (fmap DigestField . hashHandle)

fetchInfo :: (FromJSON l) => FilePath -> IO (FetchInfo l)
fetchInfo p = do
  meta <- Json.decodeStrictBsThrow <$> BS.readFile (p </> "meta.json")
  bracket
    (P.FilePath.openFd p P.FilePath.ReadOnly P.FilePath.defaultFileFlags)
    P.FilePath.closeFd
    ( \fd -> do
        keyLog <- hashFile fd "sslkeylog.keys"
        logFile <- hashFile fd "log.bin"
        reqMeta <- hashFile fd "request_meta.tar"
        reqData <- hashFile fd "traffic.pcapng"
        pure
          ( FetchInfo
              { tiTimestamp = fmTimestamp meta,
                tiLog = logFile,
                tiUser = fmUser meta,
                tiKeyLog = Just keyLog,
                tiTraffic =
                  TWireshark
                    { twKeyLog = keyLog,
                      twRequestMeta = reqMeta,
                      twData = reqData
                    }
              }
          )
    )

addWiresharkFetch ::
  (FromJSON l, ToCbor h, ToCbor st, ToCbor rt, ToCbor l) =>
  OS.ObjectStore h a st rt ->
  BS.BlobStore ->
  rt ->
  FilePath ->
  IO (FetchId l)
addWiresharkFetch objStore _ ty fetch =
  fetchInfo fetch >>= \info ->
    OS.objectId <$> OS.addObject objStore (OtRecord ty) 1 info
