{-# LANGUAGE LambdaCase #-}

module Webar.Codec.Cbor.Decoding
  ( Decoder,
    decodeWord8Canonical,
    decodeWord16Canonical,
    decodeWord32Canonical,
    decodeWord64Canonical,
    decodeWordCanonical,
    decodeInt8Canonical,
    decodeInt16Canonical,
    decodeInt32Canonical,
    decodeInt64Canonical,
    decodeIntCanonical,
    decodeNull,
    decodeBool,
    decodeStringCanonical,
    decodeBytesCanonical,
    decodeListLenCanonical,
    decodeListLenCanonicalOf,
    decodeMapLenCanonical,
    decodeFieldOf,
    decodeNormalProd,
    decodeNormalProdOf,
    decodeRecordProd,
    decodeRecordProdOf,
    SumKind (..),
    decodeSum,
    FromCbor (..),
  )
where

import Codec.CBOR.Decoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)

class FromCbor a where
  fromCbor :: Decoder s a

instance FromCbor Word8 where
  fromCbor = decodeWord8Canonical

instance FromCbor Word16 where
  fromCbor = decodeWord16Canonical

instance FromCbor Word32 where
  fromCbor = decodeWord32Canonical

instance FromCbor Word64 where
  fromCbor = decodeWord64Canonical

instance FromCbor Word where
  fromCbor = decodeWordCanonical

instance FromCbor Int8 where
  fromCbor = decodeInt8Canonical

instance FromCbor Int16 where
  fromCbor = decodeInt16Canonical

instance FromCbor Int32 where
  fromCbor = decodeInt32Canonical

instance FromCbor Int64 where
  fromCbor = decodeInt64Canonical

instance FromCbor Int where
  fromCbor = decodeIntCanonical

instance FromCbor () where
  fromCbor = decodeNull

instance FromCbor Void where
  fromCbor = fail "Void"

instance FromCbor Bool where
  fromCbor = decodeBool

instance FromCbor Text where
  fromCbor = decodeStringCanonical

instance FromCbor ByteString where
  fromCbor = decodeBytesCanonical

instance (FromCbor a) => FromCbor (Maybe a) where
  fromCbor =
    peekTokenType >>= \case
      TypeNull -> Nothing <$ decodeNull
      _ -> Just <$> fromCbor

instance (FromCbor a) => FromCbor (V.Vector a) where
  fromCbor =
    decodeListLenCanonical >>= \l ->
      decodeSequenceLenN
        (\revList i -> i : revList)
        []
        (\revList -> V.fromListN l (reverse revList))
        l
        fromCbor

instance (FromCbor a, Ord a) => FromCbor (S.Set a) where
  fromCbor =
    decodeListLenCanonical >>= \l ->
      decodeSequenceLenN (\s e -> S.insert e s) S.empty id l fromCbor

instance (FromCbor k, FromCbor v, Ord k) => FromCbor (M.Map k v) where
  fromCbor = decodeMapLenCanonical >>= iter M.empty
    where
      iter m 0 = pure m
      iter m i = do
        k <- fromCbor
        v <- fromCbor
        iter (M.insert k v m) (i - 1)

instance FromCbor UUID.UUID where
  fromCbor =
    decodeTagCanonical >>= \case
      37 ->
        decodeBytesCanonical >>= \b ->
          case UUID.fromByteString (LBS.fromStrict b) of
            Just u -> pure u
            Nothing -> fail "Expect uuid bytestring"
      _ -> fail "Expect uuid tag 37"

decodeFieldOf :: (FromCbor v) => Text -> Decoder s v
decodeFieldOf name =
  decodeStringCanonical >>= \f ->
    if f == name
      then fromCbor
      else fail ("unexpected field name: " ++ show f)

decodeNormalProd :: Decoder s Int
decodeNormalProd = decodeListLenCanonical

decodeNormalProdOf :: Int -> Decoder s ()
decodeNormalProdOf = decodeListLenCanonicalOf

decodeRecordProd :: Decoder s Int
decodeRecordProd = decodeMapLenCanonical

decodeRecordProdOf :: Int -> Decoder s ()
decodeRecordProdOf l =
  decodeMapLenCanonical >>= \len ->
    if l == len then pure () else fail "record size mismatch"

data SumKind
  = SkUnit Text
  | SkNormal Text {-# UNPACK #-} Int
  | SkRecord Text {-# UNPACK #-} Int

decodeSum :: Decoder s SumKind
decodeSum =
  peekTokenType >>= \case
    TypeString -> SkUnit <$> decodeStringCanonical
    TypeTag -> do
      decodeTagCanonical >>= \case
        27 -> pure ()
        _ -> fail "invalid tag, expect tag 27"
      decodeListLenCanonicalOf 2
      t <- decodeStringCanonical
      peekTokenType >>= \case
        TypeListLen -> SkNormal t <$> decodeListLenCanonical
        TypeMapLen -> SkRecord t <$> decodeMapLenCanonical
        _ -> fail "expect map or list"
    _ -> fail ": expect string or tag 27"
