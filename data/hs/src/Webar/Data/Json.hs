-- | canonicalized json interface
-- based on rfc8785, with integers written as decimal
module Webar.Data.Json
  ( ToJSON (..),
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word

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
