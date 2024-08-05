{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data is not add to store now, but may change in the future, so
--    `DigestField` is only exported in the internal module
module Webar.Fetch.Http
  ( KeyLogId,
    WiresharkDataId,
    LogId,
    RequestMetaId,
    idToDigest,
    Traffic (..),
    FetchInfo (..),
    FetchId,
  )
where

import Webar.Data.TH
import Webar.Fetch.Http.Internal
import Webar.Object
import Webar.Types (Timestamp)

type KeyLogId = DigestField KeyLog

type WiresharkDataId = DigestField WiresharkData

type LogId = DigestField Log

type RequestMetaId = DigestField RequestMeta

data Traffic = TWireshark
  { twKeyLog :: KeyLogId,
    twRequestMeta :: RequestMetaId,
    twData :: WiresharkDataId
  }
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Traffic

data FetchInfo l = FetchInfo
  { tiTimestamp :: Timestamp,
    tiLog :: LogId,
    tiUser :: Maybe l,
    tiKeyLog :: Maybe KeyLogId,
    tiTraffic :: Traffic
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchInfo

type FetchId l = ObjectId (FetchInfo l)