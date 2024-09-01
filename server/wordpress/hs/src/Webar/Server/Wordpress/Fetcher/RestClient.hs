{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.Fetcher.RestClient where

import Data.Word (Word32)
import Webar.Data.Cbor.TH
import Webar.Http (JsonBody)
import Webar.Server.Wordpress.RestApi.Source
import Webar.Server.Wordpress.RestApi.Types (ApiInfo)

data HttpRequest = HttpRequest
  { hrRequest :: Request,
    hrResponse :: Response JsonBody
  }

deriveProdFromCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpRequest

data ApiResponse = ApiResponse
  { arApi :: ApiInfo,
    arSeq :: Word32,
    arData :: ResponseData HttpRequest
  }

deriveProdFromCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiResponse