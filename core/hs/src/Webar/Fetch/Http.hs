{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data is not add to store now, but may change in the future, so
--    `DigestField` is only exported in the internal module
module Webar.Fetch.Http
  ( KeyLogId,
    WiresharkDataId,
    LogId,
    RequestMetaId,
    FetchDataId,
    idToDigest,
    Traffic (..),
    TrafficType (..),
    Metadata (..),
    FetchInfo (..),
    FetchId,
  )
where

import Webar.Data.TH
import Webar.Fetch.Http.Internal
import Webar.Object (ObjectId)
import Webar.Types (Timestamp)

type KeyLogId = DigestField KeyLog

type WiresharkDataId = DigestField WiresharkData

type DumpcapLogId = DigestField DumpcapLog

type LogId = DigestField Log

type RequestMetaId = DigestField RequestMeta

type FetchDataId = DigestField FetchData

-- | Fetch traffic
--
--  Since fetched data can be regenerated from captured traffic and metadata,
--  it is recorded only if no traffic is captured
data Traffic
  = TWireshark
      { twKeyLog :: KeyLogId,
        twRequestMeta :: RequestMetaId,
        twDumpcapLog :: DumpcapLogId,
        twData :: WiresharkDataId
      }
  | -- | Traffic is not captured, so use fetched data to distinguish different
    -- fetch
    TNone {tnFetchData :: FetchDataId}
  deriving (Show)

deriveSumData
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Traffic

data TrafficType = TtWireshark
  deriving (Show)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''TrafficType

data Metadata l = Metadata
  { metaTraffic :: Maybe TrafficType,
    metaStartTime :: Timestamp,
    metaEndTime :: Timestamp,
    metaUser :: l
  }

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''Metadata

data FetchInfo l = FetchInfo
  { tiStartTime :: Timestamp,
    tiEndTime :: Timestamp,
    tiLog :: LogId,
    tiUser :: l,
    tiTraffic :: Traffic
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchInfo

type FetchId l = ObjectId (FetchInfo l)
