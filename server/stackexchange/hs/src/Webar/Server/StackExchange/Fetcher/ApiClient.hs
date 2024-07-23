{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Fetcher.ApiClient where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Webar.Data.Cbor.TH
import Webar.Http (Method)
import Webar.Server.StackExchange.Api.Source
import Webar.Server.StackExchange.Types (ApiSiteParameter, FilterId)

data HttpRequest = HttpRequest
  { hrMethod :: Method,
    hrUrl :: Text,
    hrRequestId :: RequestId,
    hrResponse :: Response ByteString
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
