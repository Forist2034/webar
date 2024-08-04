{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Fetch.Http
  ( Traffic (..),
    FetchInfo (..),
    FetchId,
  )
where

import Webar.Data.TH
import Webar.Object
import Webar.Types (Timestamp)

data Traffic = TWireshark
  { twKeyLog :: DataId,
    twRequestMeta :: DataId,
    twData :: DataId
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
    tiLog :: DataId,
    tiUser :: Maybe l,
    tiKeyLog :: Maybe DataId,
    tiTraffic :: Traffic
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FetchInfo

type FetchId l = ObjectId (FetchInfo l)