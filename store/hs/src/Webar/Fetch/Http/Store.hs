{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Fetch.Http.Store (addWiresharkFetch) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import System.FilePath
import qualified System.Posix as P.FilePath
import Webar.Data.Cbor (ToCbor)
import Webar.Data.Json (FromJSON)
import qualified Webar.Data.Json as Json
import Webar.Data.Json.TH (ProductOptions (..), camelTo2, deriveProdFromJSON)
import Webar.Fetch.Http
import Webar.Object
import qualified Webar.Store.Data.WithShared as DS
import qualified Webar.Store.Object.Website as OS
import Webar.Types

data FetchMeta l = FetchMeta
  { fmTimestamp :: Timestamp,
    fmUser :: Maybe l
  }
  deriving (Show)

deriveProdFromJSON
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchMeta

fetchInfo :: (FromJSON l) => DS.DataStore -> FilePath -> IO (FetchInfo l)
fetchInfo store p = do
  meta <- Json.decodeStrictBsThrow <$> BS.readFile (p </> "meta.json")
  bracket
    (P.FilePath.openFd p P.FilePath.ReadOnly P.FilePath.defaultFileFlags)
    P.FilePath.closeFd
    ( \fd -> do
        keyLog <- DS.dataId <$> DS.addFileByRawPathAt store fd "sslkeylog.keys"
        logFile <- DS.dataId <$> DS.addFileByRawPathAt store fd "log.bin"
        reqMeta <- DS.dataId <$> DS.addFileByRawPathAt store fd "request_meta.tar"
        reqData <- DS.dataId <$> DS.addFileByRawPathAt store fd "traffic.pcapng"
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
  DS.DataStore ->
  rt ->
  FilePath ->
  IO (FetchId l)
addWiresharkFetch objStore dataStore ty fetch =
  fetchInfo dataStore fetch >>= \info ->
    OS.objectId <$> OS.addObject objStore (OtRecord ty) 1 info