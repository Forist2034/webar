{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Types
  ( Timestamp (..),
    Version (..),
    Server (..),
    Domain,
    domainText,
    Host (..),
  )
where

import Codec.CBOR.Decoding (decodeListLenCanonicalOf, decodeStringCanonical)
import Codec.CBOR.Encoding
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import Webar.Data.Cbor
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH

data Timestamp = Timestamp
  { tsSecs :: {-# UNPACK #-} !Word64,
    tsNanos :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq, Ord)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Timestamp

data Version = Version {-# UNPACK #-} Word8 {-# UNPACK #-} Word8
  deriving (Show, Eq, Ord)

deriveProdData defaultProductOptions ''Version

data Server = Server
  { serverName :: {-# UNPACK #-} Text,
    -- | version of instance and type info
    serverVersion :: {-# UNPACK #-} Version
  }
  deriving (Show, Eq)

instance ToCbor Server where
  toCbor s =
    encodeListLen 2
      <> (encodeString (serverName s) <> toCbor (serverVersion s))

instance FromCbor Server where
  fromCbor =
    decodeListLenCanonicalOf 2
      >> Server <$> decodeStringCanonical <*> fromCbor

newtype Domain = Domain {domainText :: Text}
  deriving (Show, Eq, FromCbor, ToCbor, FromJSON, ToJSON)

newtype Host = HDomain Domain
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . tail
    }
  ''Host
