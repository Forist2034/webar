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

import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Webar.Data.Cbor (FromCbor (..), ToCbor (..))
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import Webar.Digest

data Server = Server
  { serverName :: {-# UNPACK #-} Text,
    -- | webar store and object version
    serverVersion :: {-# UNPACK #-} Word8
  }
  deriving (Show, Eq)

instance ToCbor Server where
  toCbor s =
    Cbor.encodeListLen 2
      <> ( Cbor.encodeString (serverName s)
             <> Cbor.encodeWord8 (serverVersion s)
         )

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
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ObjectType

data ObjectInfo h archive st rt = ObjectInfo
  { oiInstance :: h,
    oiType :: ObjectType archive st rt,
    oiVersion :: Word8
  }
  deriving (Show, Eq)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
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
