{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.RestApi.Types
  ( ApiVersion (..),
    ApiInfo (..),
    module T,
  )
where

import Webar.Data.TH
import Webar.Server.StackExchange.RestApi.Filter (FilterId)
import Webar.Server.StackExchange.RestApi.Internal.Version
import Webar.Server.StackExchange.Types as T

data ApiInfo = ApiInfo
  { aiVersion :: ApiVersion,
    aiFilter :: FilterId
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiInfo