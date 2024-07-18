-- | canonicalized json interface
-- based on rfc8785, with integers written as decimal
module Webar.Data.Json
  ( ToJSONKey (..),
    ToJSON (..),
    Aeson.FromJSONKey (..),
    Aeson.FromJSON (..),
    encodeBuilder,
    encodeLazyBs,
    encodeStrictBs,
    decodeLazyBS,
    decodeStrictBs,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson.Decoding
import Data.Aeson.Encoding
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word

class ToJSONKey a where
  toJsonKey :: a -> Encoding' KM.Key

instance ToJSONKey Int where
  toJsonKey = intText

instance ToJSONKey Int8 where
  toJsonKey = int8Text

instance ToJSONKey Int16 where
  toJsonKey = int16Text

instance ToJSONKey Int32 where
  toJsonKey = int32Text

instance ToJSONKey Int64 where
  toJsonKey = int64Text

instance ToJSONKey Word where
  toJsonKey = wordText

instance ToJSONKey Word8 where
  toJsonKey = word8Text

instance ToJSONKey Word16 where
  toJsonKey = word16Text

instance ToJSONKey Word32 where
  toJsonKey = word32Text

instance ToJSONKey Word64 where
  toJsonKey = word64Text

instance ToJSONKey Text where
  toJsonKey = text

class ToJSON a where
  toJson :: a -> Encoding

instance ToJSON Int where
  toJson = int

instance ToJSON Int8 where
  toJson = int8

instance ToJSON Int16 where
  toJson = int16

instance ToJSON Int32 where
  toJson = int32

instance ToJSON Int64 where
  toJson = int64

instance ToJSON Word where
  toJson = word

instance ToJSON Word8 where
  toJson = word8

instance ToJSON Word16 where
  toJson = word16

instance ToJSON Word32 where
  toJson = word32

instance ToJSON Word64 where
  toJson = word64

instance ToJSON Bool where
  toJson = bool

instance ToJSON Text where
  toJson = text

instance ToJSON () where
  toJson _ = null_

instance ToJSONKey UUID.UUID where
  toJsonKey u = text (UUID.toText u)

instance ToJSON UUID.UUID where
  toJson u = text (UUID.toText u)

instance (ToJSON a) => ToJSON (Maybe a) where
  toJson Nothing = null_
  toJson (Just a) = toJson a

instance (ToJSON a) => ToJSON (Set a) where
  toJson s = list toJson (S.toAscList s)

instance ToJSON IntSet where
  toJson s = list toJson (IS.toAscList s)

instance (ToJSON a) => ToJSON (V.Vector a) where
  toJson v = list toJson (V.toList v)

instance (ToJSONKey k, ToJSON v) => ToJSON (M.Map k v) where
  toJson m =
    pairs
      ( M.foldlWithKey'
          (\e k v -> e <> pair' (toJsonKey k) (toJson v))
          mempty
          m
      )

encodeBuilder :: (ToJSON a) => a -> BSB.Builder
encodeBuilder v = fromEncoding (toJson v)

encodeLazyBs :: (ToJSON a) => a -> LBS.ByteString
encodeLazyBs = BSB.toLazyByteString . encodeBuilder

encodeStrictBs :: (ToJSON a) => a -> BS.ByteString
encodeStrictBs = LBS.toStrict . encodeLazyBs

decodeLazyBS :: (Aeson.FromJSON a) => LBS.ByteString -> Either String a
decodeLazyBS = eitherDecode

decodeStrictBs :: (Aeson.FromJSON a) => BS.ByteString -> Either String a
decodeStrictBs = eitherDecodeStrict
