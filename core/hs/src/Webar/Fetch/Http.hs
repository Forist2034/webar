{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Fetch.Http
  ( FetchId (..),
    Traffic (..),
    FetchInfo (..),
  )
where

import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import Webar.Digest
import Webar.Types (Timestamp)

newtype FetchId = FetchId Digest
  deriving (Show, Eq, Ord, FromCbor, ToCbor, FromJSON, ToJSON)

data Traffic = TWireshark
  { twKeyLog :: Digest,
    twRequestMeta :: Digest,
    twData :: Digest
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
    tiLog :: Digest,
    tiUser :: Maybe l,
    tiKeyLog :: Maybe Digest,
    tiTraffic :: Traffic
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchInfo
