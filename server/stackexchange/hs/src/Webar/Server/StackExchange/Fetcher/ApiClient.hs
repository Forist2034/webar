{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Fetcher.ApiClient where

import Data.Text (Text)
import Data.Word (Word32)
import Webar.Data.Cbor.TH
import Webar.Http (JsonBody, Method)
import Webar.Server.StackExchange.Api.Filter (FilterId)
import Webar.Server.StackExchange.Api.Request
import Webar.Server.StackExchange.Api.Types (ApiSiteParameter, ApiVersion)

data HttpRequest = HttpRequest
  { hrMethod :: Method,
    hrUrl :: Text,
    hrRequestId :: RequestId,
    hrResponse :: Response JsonBody
  }
  deriving (Show)

deriveProdFromCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpRequest

data ApiResponse = ApiResponse
  { arApiVersion :: ApiVersion,
    arSite :: ApiSiteParameter,
    arFilter :: FilterId,
    arSeq :: Word32,
    arData :: ResponseData HttpRequest
  }
  deriving (Show)

deriveProdFromCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiResponse
