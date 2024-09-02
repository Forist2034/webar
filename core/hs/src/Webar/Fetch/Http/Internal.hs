{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Webar.Fetch.Http.Internal where

import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Digest (Digest)

data KeyLog

data TracingLog

data WiresharkData

data DumpcapLog

data RequestMeta

data FetchData

newtype DigestField t = DigestField {idToDigest :: Digest}
  deriving (Show, Eq, FromCbor, ToCbor, FromJSON, ToJSON)
