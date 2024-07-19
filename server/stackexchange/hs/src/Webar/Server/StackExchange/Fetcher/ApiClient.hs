{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Fetcher.ApiClient where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Webar.Data.Cbor.TH
import Webar.Server.StackExchange.Api.Source
import Webar.Server.StackExchange.Types (FilterId)
import Webar.Types (Timestamp)

data HttpRequest = HttpRequest
  { hrUrl :: Text,
    hrTimestamp :: Timestamp,
    hrRequestId :: RequestId,
    hrResponse :: Response ByteString
  }
  deriving (Show)

deriveProdFromCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpRequest

data ApiResponse = ApiResponse
  { arApiVersion :: ApiVersion,
    arFilter :: FilterId,
    arData :: ResponseData HttpRequest
  }
  deriving (Show)

deriveProdFromCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiResponse
