{-# LANGUAGE LambdaCase #-}

module Webar.Data.Cbor
  ( ToCbor (..),
    FromCbor (..),
    encodeBuilder,
    encodeLazyBs,
    encodeStrictBs,
    decodeLazyBs,
    decodeStrictBs,
    CborError,
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
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void
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

instance ToCbor () where
  toCbor _ = encodeNull

instance FromCbor () where
  fromCbor = decodeNull

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

instance ToCbor Void where
  toCbor = absurd

instance FromCbor Void where
  fromCbor = fail "Void"

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

decodeList :: (FromCbor a) => Int -> Decoder s [a]
decodeList 0 = pure []
decodeList n = liftA2 (:) fromCbor (decodeList (n - 1))

instance (FromCbor a) => FromCbor (V.Vector a) where
  fromCbor = decodeListLen >>= \l -> V.fromListN l <$> decodeList l

instance (ToCbor a) => ToCbor [a] where
  toCbor l =
    foldl'
      (\e a -> e <> toCbor a)
      (encodeListLen (fromIntegral (length l)))
      l

instance (FromCbor a) => FromCbor [a] where
  fromCbor = decodeListLenCanonical >>= \l -> decodeList l

instance (ToCbor a) => ToCbor (S.Set a) where
  toCbor s = S.foldl' (\e a -> e <> toCbor a) (encodeListLen (fromIntegral (S.size s))) s

instance (Ord a, FromCbor a) => FromCbor (S.Set a) where
  fromCbor = decodeListLen >>= go S.empty
    where
      go acc 0 = pure acc
      go acc n =
        fromCbor >>= \h ->
          go (S.insert h acc) (n - 1)

instance (Ord k, ToCbor k, ToCbor v) => ToCbor (M.Map k v) where
  toCbor m =
    M.foldlWithKey'
      (\e k v -> e <> toCbor k <> toCbor v)
      (encodeMapLen (fromIntegral (M.size m)))
      m

instance (Ord k, FromCbor k, FromCbor v) => FromCbor (M.Map k v) where
  fromCbor = decodeMapLen >>= go M.empty
    where
      go acc 0 = pure acc
      go acc n = do
        k <- fromCbor
        v <- fromCbor
        go (M.insert k v acc) (n - 1)

encodeBuilder :: (ToCbor a) => a -> BSB.Builder
encodeBuilder a = toBuilder (toCbor a)

encodeLazyBs :: (ToCbor a) => a -> LBS.ByteString
encodeLazyBs a = toLazyByteString (toCbor a)

encodeStrictBs :: (ToCbor a) => a -> ByteString
encodeStrictBs a = toStrictByteString (toCbor a)

decodeLazyBs :: (FromCbor a) => LBS.ByteString -> Either DeserialiseFailure (LBS.ByteString, a)
decodeLazyBs = deserialiseFromBytes fromCbor

decodeStrictBs :: (FromCbor a) => ByteString -> Either DeserialiseFailure (LBS.ByteString, a)
decodeStrictBs = decodeLazyBs . LBS.fromStrict

newtype CborError = DecodeFailure DeserialiseFailure
  deriving (Show)

instance Exception CborError

decodeLazyBsThrow :: (FromCbor a) => LBS.ByteString -> a
decodeLazyBsThrow b = case decodeLazyBs b of
  Right (_, v) -> v
  Left e -> throw (DecodeFailure e)

decodeStrictBsThrow :: (FromCbor a) => ByteString -> a
decodeStrictBsThrow b = case decodeStrictBs b of
  Right (_, v) -> v
  Left e -> throw (DecodeFailure e)
