{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Webar.Fetch.Http.Internal where

import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Digest (Digest)

data KeyLog

data Log

data WiresharkData

data RequestMeta

newtype DigestField t = DigestField {idToDigest :: Digest}
  deriving (Show, Eq, FromCbor, ToCbor, FromJSON, ToJSON)
