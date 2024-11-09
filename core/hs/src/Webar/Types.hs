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

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import Webar.Codec.Cbor.Internal.Decoding
import Webar.Codec.Cbor.Internal.Encoding
import Webar.Codec.Cbor.TH
import qualified Webar.Data.Cbor as Data
import Webar.Data.Json (FromJSON, ToJSON)
import qualified Webar.Data.TH as Data

data Timestamp = Timestamp
  { tsSecs :: {-# UNPACK #-} !Word64,
    tsNanos :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq, Ord)

Data.deriveProdData
  Data.defaultProductOptions {Data.fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Timestamp

data Version = Version {-# UNPACK #-} Word8 {-# UNPACK #-} Word8
  deriving (Show, Eq, Ord)

Data.deriveProdData Data.defaultProductOptions ''Version

deriveProductCbor defaultProductOptions ''Version

data Server = Server
  { serverName :: {-# UNPACK #-} Text,
    -- | version of instance and type info
    serverVersion :: {-# UNPACK #-} Version
  }
  deriving (Show, Eq)

instance Data.ToCbor Server where
  toCbor s =
    Enc.encodeListLen 2
      <> (Enc.encodeString (serverName s) <> Data.toCbor (serverVersion s))

instance Data.FromCbor Server where
  fromCbor =
    Dec.decodeListLenCanonicalOf 2
      >> Server <$> Dec.decodeStringCanonical <*> Data.fromCbor

instance ToCbor Server where
  toCbor s =
    Encoding
      ( Enc.encodeListLen 2
          <> Enc.encodeString (serverName s)
          <> getEncoding (toCbor (serverVersion s))
      )

instance FromCbor Server where
  fromCbor =
    Decoder (Dec.decodeListLenCanonicalOf 2)
      >> Server <$> Decoder Dec.decodeStringCanonical <*> fromCbor

newtype Domain = Domain {domainText :: Text}
  deriving (Show, Eq, Data.FromCbor, Data.ToCbor, FromCbor, ToCbor, FromJSON, ToJSON)

newtype Host = HDomain Domain
  deriving (Show, Eq)

Data.deriveSumData
  Data.defaultSumOptions
    { Data.constructorTagModifier = camelTo2 '_' . tail
    }
  ''Host
