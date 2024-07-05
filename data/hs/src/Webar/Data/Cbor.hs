{-# LANGUAGE LambdaCase #-}

module Webar.Data.Cbor
  ( ToCbor (..),
    FromCbor (..),
    encodeStrictBs,
    decodeLazyBs,
    decodeStrictBs,
    decodeLazyBsThrow,
    decodeStrictBsThrow,
  )
where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Applicative (Applicative (liftA2))
import Control.Exception (Exception, throw)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word

class ToCbor a where
  toCbor :: a -> Encoding

class FromCbor a where
  fromCbor :: Decoder s a

instance ToCbor Word8 where
  toCbor = encodeWord8

instance FromCbor Word8 where
  fromCbor = decodeWord8

instance ToCbor Word16 where
  toCbor = encodeWord16

instance FromCbor Word16 where
  fromCbor = decodeWord16

instance ToCbor Word32 where
  toCbor = encodeWord32

instance FromCbor Word32 where
  fromCbor = decodeWord32

instance ToCbor Word64 where
  toCbor = encodeWord64

instance FromCbor Word64 where
  fromCbor = decodeWord64

instance ToCbor Word where
  toCbor = encodeWord

instance FromCbor Word where
  fromCbor = decodeWord

instance ToCbor Int8 where
  toCbor = encodeInt8

instance FromCbor Int8 where
  fromCbor = decodeInt8

instance ToCbor Int16 where
  toCbor = encodeInt16

instance FromCbor Int16 where
  fromCbor = decodeInt16

instance ToCbor Int32 where
  toCbor = encodeInt32

instance FromCbor Int32 where
  fromCbor = decodeInt32

instance ToCbor Int64 where
  toCbor = encodeInt64

instance FromCbor Int64 where
  fromCbor = decodeInt64

instance ToCbor Int where
  toCbor = encodeInt

instance FromCbor Int where
  fromCbor = decodeInt

instance ToCbor Text where
  toCbor = encodeString

instance FromCbor Text where
  fromCbor = decodeStringCanonical

instance ToCbor Bool where
  toCbor = encodeBool

instance FromCbor Bool where
  fromCbor = decodeBool

instance ToCbor ByteString where
  toCbor = encodeBytes

instance FromCbor ByteString where
  fromCbor = decodeBytesCanonical

instance ToCbor UUID.UUID where
  toCbor u = encodeBytes (LBS.toStrict (UUID.toByteString u))

instance FromCbor UUID.UUID where
  fromCbor =
    decodeBytesCanonical >>= \b ->
      case UUID.fromByteString (LBS.fromStrict b) of
        Just u -> pure u
        Nothing -> fail "invalid uuid"

instance (ToCbor a) => ToCbor (Maybe a) where
  toCbor Nothing = encodeNull
  toCbor (Just v) = toCbor v

instance (FromCbor a) => FromCbor (Maybe a) where
  fromCbor =
    peekTokenType >>= \case
      TypeNull -> Nothing <$ decodeNull
      _ -> Just <$> fromCbor

instance (ToCbor a) => ToCbor (V.Vector a) where
  toCbor v = V.foldl' (\e a -> e <> toCbor a) (encodeListLen (fromIntegral (V.length v))) v

instance (FromCbor a) => FromCbor (V.Vector a) where
  fromCbor = decodeListLen >>= \l -> V.fromListN l <$> go l
    where
      go 0 = pure []
      go n = liftA2 (:) fromCbor (go (n - 1))

instance (ToCbor a) => ToCbor (S.Set a) where
  toCbor s = S.foldl' (\e a -> e <> toCbor a) (encodeListLen (fromIntegral (S.size s))) s

instance (Ord a, FromCbor a) => FromCbor (S.Set a) where
  fromCbor = decodeListLen >>= go S.empty
    where
      go acc 0 = pure acc
      go acc n =
        fromCbor >>= \h ->
          go (S.insert h acc) (n - 1)

encodeStrictBs :: (ToCbor a) => a -> ByteString
encodeStrictBs a = toStrictByteString (toCbor a)

decodeLazyBs :: (FromCbor a) => LBS.ByteString -> Either DeserialiseFailure (LBS.ByteString, a)
decodeLazyBs = deserialiseFromBytes fromCbor

decodeStrictBs :: (FromCbor a) => ByteString -> Either DeserialiseFailure (LBS.ByteString, a)
decodeStrictBs = decodeLazyBs . LBS.fromStrict

newtype DecodeError = DecodeFailure DeserialiseFailure
  deriving (Show)

instance Exception DecodeError

decodeLazyBsThrow :: (FromCbor a) => LBS.ByteString -> a
decodeLazyBsThrow b = case decodeLazyBs b of
  Right (_, v) -> v
  Left e -> throw (DecodeFailure e)

decodeStrictBsThrow :: (FromCbor a) => ByteString -> a
decodeStrictBsThrow b = case decodeStrictBs b of
  Right (_, v) -> v
  Left e -> throw (DecodeFailure e)
