{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Webar.Codec.Cbor.Internal.Encoding
  ( Encoding (..),
    encodeNormalProd,
    encodeRecordProd,
    encodeUnitSum,
    encodeNormalSum,
    encodeRecordSum,
    ToCbor (..),
    encodeField,
  )
where

import qualified Codec.CBOR.Encoding as Enc
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)

newtype Encoding = Encoding {getEncoding :: Enc.Encoding}
  deriving (Semigroup, Monoid)

encodeNormalProd :: Word -> Encoding
encodeNormalProd = coerce Enc.encodeListLen

encodeRecordProd :: Word -> Encoding
encodeRecordProd = coerce Enc.encodeMapLen

encodeUnitSum :: Text -> Encoding
encodeUnitSum = coerce Enc.encodeString

encodeNormalSum :: Text -> Word -> Encoding
encodeNormalSum t l =
  Encoding
    ( Enc.encodeTag 27
        <> Enc.encodeListLen 2
        <> Enc.encodeString t
        <> Enc.encodeListLen l
    )

encodeRecordSum :: Text -> Word -> Encoding
encodeRecordSum t l =
  Encoding
    ( Enc.encodeTag 27
        <> Enc.encodeListLen 2
        <> Enc.encodeString t
        <> Enc.encodeMapLen l
    )

class ToCbor a where
  toCbor :: a -> Encoding

instance ToCbor Word8 where
  toCbor = coerce Enc.encodeWord8

instance ToCbor Word16 where
  toCbor = coerce Enc.encodeWord16

instance ToCbor Word32 where
  toCbor = coerce Enc.encodeWord32

instance ToCbor Word64 where
  toCbor = coerce Enc.encodeWord64

instance ToCbor Word where
  toCbor = coerce Enc.encodeWord

instance ToCbor Int8 where
  toCbor = coerce Enc.encodeInt8

instance ToCbor Int16 where
  toCbor = coerce Enc.encodeInt16

instance ToCbor Int32 where
  toCbor = coerce Enc.encodeInt32

instance ToCbor Int64 where
  toCbor = coerce Enc.encodeInt64

instance ToCbor Int where
  toCbor = coerce Enc.encodeInt

instance ToCbor () where
  toCbor _ = coerce Enc.encodeNull

instance ToCbor Void where
  toCbor = absurd

instance ToCbor Bool where
  toCbor = coerce Enc.encodeBool

instance ToCbor Text where
  toCbor = coerce Enc.encodeString

instance ToCbor ByteString where
  toCbor = coerce Enc.encodeBytes

instance (ToCbor a) => ToCbor (Maybe a) where
  toCbor (Just a) = toCbor a
  toCbor Nothing = coerce Enc.encodeNull

instance (ToCbor a) => ToCbor (V.Vector a) where
  toCbor v =
    V.foldl'
      (\e a -> e <> toCbor a)
      (Encoding (Enc.encodeListLen (fromIntegral (V.length v))))
      v

instance (ToCbor a) => ToCbor (S.Set a) where
  toCbor s =
    S.foldl'
      (\e a -> e <> toCbor a)
      (Encoding (Enc.encodeListLen (fromIntegral (S.size s))))
      s

instance (ToCbor k, ToCbor v) => ToCbor (M.Map k v) where
  toCbor m =
    M.foldlWithKey'
      (\e k v -> e <> toCbor k <> toCbor v)
      (Encoding (Enc.encodeMapLen (fromIntegral (M.size m))))
      m

instance ToCbor UUID.UUID where
  toCbor u = Encoding (Enc.encodeTag 37 <> Enc.encodeBytes (LBS.toStrict (UUID.toByteString u)))

encodeField :: (ToCbor v) => Text -> v -> Encoding
encodeField k v = Encoding (Enc.encodeString k) <> toCbor v
