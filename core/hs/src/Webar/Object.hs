{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--  Object type definition.
--
--  terminology:
--
--  [@server@]: e.g. wordpress
--
--  [@instance@]: e.g. example.wordpress.com
--
--  [@archive@]: e.g. post
--
--  [@record@]: e.g. fetch
--
--  [@snapshot@]: one fetched data of a @archive@
module Webar.Object
  ( Server (..),
    ObjectId (..),
    ObjectType (..),
    ObjectInfo (..),
    encodeObject,
  )
where

import Codec.CBOR.Decoding (decodeListLenCanonicalOf, decodeStringCanonical)
import Codec.CBOR.Encoding
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import Data.ByteString (ByteString)
import Data.Text (Text)
import Webar.Data.Cbor (FromCbor (..), ToCbor (..))
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import Webar.Digest
import Webar.Types (Version)

data Server = Server
  { serverName :: {-# UNPACK #-} Text,
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

newtype ObjectId t = ObjectId Digest
  deriving (Show, Eq, Ord, FromCbor, ToCbor, FromJSON, ToJSON)

data ObjectType archive st rt
  = OtSnapshot
      { otArchive :: ObjectId archive,
        -- | snapshot info type
        otType :: st
      }
  | OtRecord rt
  | OtArchive
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ObjectType

data ObjectInfo h archive st rt = ObjectInfo
  { oiInstance :: h,
    oiType :: ObjectType archive st rt,
    oiVersion :: Version
  }
  deriving (Show, Eq)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ObjectInfo

encodeObject ::
  (ToCbor h, ToCbor so, ToCbor st, ToCbor b) =>
  Server ->
  ObjectInfo h a so st ->
  b ->
  ByteString
encodeObject server info body =
  Cbor.toStrictByteString
    ( Cbor.encodeMapLen 4
        <> (Cbor.encodeString "version" <> Cbor.encodeWord 1)
        <> (Cbor.encodeString "server" <> toCbor server)
        <> (Cbor.encodeString "info" <> toCbor info)
        <> (Cbor.encodeString "data" <> toCbor body)
    )
