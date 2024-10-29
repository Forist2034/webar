module Webar.Codec.Cbor.Encoding
  ( Encoding,
    encodeWord8,
    encodeWord16,
    encodeWord32,
    encodeWord64,
    encodeWord,
    encodeInt8,
    encodeInt16,
    encodeInt32,
    encodeInt64,
    encodeInt,
    encodeNull,
    encodeBool,
    encodeString,
    encodeBytes,
    encodeListLen,
    encodeMapLen,
    encodeNormalProd,
    encodeRecordProd,
    encodeUnitSum,
    encodeNormalSum,
    encodeRecordSum,
    ToCbor (..),
    encodeField,
  )
where

import Codec.CBOR.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)

encodeNormalProd :: Word -> Encoding
encodeNormalProd = encodeListLen

encodeRecordProd :: Word -> Encoding
encodeRecordProd = encodeMapLen

encodeUnitSum :: Text -> Encoding
encodeUnitSum = encodeString

encodeNormalSum :: Text -> Word -> Encoding
encodeNormalSum t l =
  encodeTag 27
    <> encodeListLen 2
    <> encodeString t
    <> encodeListLen l

encodeRecordSum :: Text -> Word -> Encoding
encodeRecordSum t l =
  encodeTag 27
    <> encodeListLen 2
    <> encodeString t
    <> encodeMapLen l

class ToCbor a where
  toCbor :: a -> Encoding

instance ToCbor Word8 where
  toCbor = encodeWord8

instance ToCbor Word16 where
  toCbor = encodeWord16

instance ToCbor Word32 where
  toCbor = encodeWord32

instance ToCbor Word64 where
  toCbor = encodeWord64

instance ToCbor Word where
  toCbor = encodeWord

instance ToCbor Int8 where
  toCbor = encodeInt8

instance ToCbor Int16 where
  toCbor = encodeInt16

instance ToCbor Int32 where
  toCbor = encodeInt32

instance ToCbor Int64 where
  toCbor = encodeInt64

instance ToCbor Int where
  toCbor = encodeInt

instance ToCbor () where
  toCbor _ = encodeNull

instance ToCbor Void where
  toCbor = absurd

instance ToCbor Bool where
  toCbor = encodeBool

instance ToCbor Text where
  toCbor = encodeString

instance ToCbor ByteString where
  toCbor = encodeBytes

instance (ToCbor a) => ToCbor (Maybe a) where
  toCbor (Just a) = toCbor a
  toCbor Nothing = encodeNull

instance (ToCbor a) => ToCbor (V.Vector a) where
  toCbor v =
    V.foldl'
      (\e a -> e <> toCbor a)
      (encodeListLen (fromIntegral (V.length v)))
      v

instance (ToCbor a) => ToCbor (S.Set a) where
  toCbor s =
    S.foldl'
      (\e a -> e <> toCbor a)
      (encodeListLen (fromIntegral (S.size s)))
      s

instance (ToCbor k, ToCbor v) => ToCbor (M.Map k v) where
  toCbor m =
    M.foldlWithKey'
      (\e k v -> e <> toCbor k <> toCbor v)
      (encodeMapLen (fromIntegral (M.size m)))
      m

instance ToCbor UUID.UUID where
  toCbor u = encodeTag 37 <> encodeBytes (LBS.toStrict (UUID.toByteString u))

encodeField :: (ToCbor v) => Text -> v -> Encoding
encodeField k v = encodeString k <> toCbor v