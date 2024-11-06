{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Webar.Codec.Cbor.Internal.Decoding
  ( Decoder (..),
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

import qualified Codec.CBOR.Decoding as Dec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)

newtype Decoder s a = Decoder {getDecoder :: Dec.Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

class FromCbor a where
  fromCbor :: Decoder s a

instance FromCbor Word8 where
  fromCbor = Decoder Dec.decodeWord8Canonical

instance FromCbor Word16 where
  fromCbor = Decoder Dec.decodeWord16Canonical

instance FromCbor Word32 where
  fromCbor = Decoder Dec.decodeWord32Canonical

instance FromCbor Word64 where
  fromCbor = Decoder Dec.decodeWord64Canonical

instance FromCbor Word where
  fromCbor = Decoder Dec.decodeWordCanonical

instance FromCbor Int8 where
  fromCbor = Decoder Dec.decodeInt8Canonical

instance FromCbor Int16 where
  fromCbor = Decoder Dec.decodeInt16Canonical

instance FromCbor Int32 where
  fromCbor = Decoder Dec.decodeInt32Canonical

instance FromCbor Int64 where
  fromCbor = Decoder Dec.decodeInt64Canonical

instance FromCbor Int where
  fromCbor = Decoder Dec.decodeIntCanonical

instance FromCbor () where
  fromCbor = Decoder Dec.decodeNull

instance FromCbor Void where
  fromCbor = fail "Void"

instance FromCbor Bool where
  fromCbor = Decoder Dec.decodeBool

instance FromCbor Text where
  fromCbor = Decoder Dec.decodeStringCanonical

instance FromCbor ByteString where
  fromCbor = Decoder Dec.decodeBytesCanonical

instance (FromCbor a) => FromCbor (Maybe a) where
  fromCbor =
    Decoder Dec.peekTokenType >>= \case
      Dec.TypeNull -> Nothing <$ Decoder Dec.decodeNull
      _ -> Just <$> fromCbor

instance (FromCbor a) => FromCbor (V.Vector a) where
  fromCbor =
    Decoder
      ( Dec.decodeListLenCanonical >>= \l ->
          Dec.decodeSequenceLenN
            (\revList i -> i : revList)
            []
            (\revList -> V.fromListN l (reverse revList))
            l
            (getDecoder fromCbor)
      )

instance (FromCbor a, Ord a) => FromCbor (S.Set a) where
  fromCbor =
    Decoder
      ( Dec.decodeListLenCanonical >>= \l ->
          Dec.decodeSequenceLenN (\s e -> S.insert e s) S.empty id l (getDecoder fromCbor)
      )

instance (FromCbor k, FromCbor v, Ord k) => FromCbor (M.Map k v) where
  fromCbor = Decoder Dec.decodeMapLenCanonical >>= iter M.empty
    where
      iter m 0 = pure m
      iter m i = do
        k <- fromCbor
        v <- fromCbor
        iter (M.insert k v m) (i - 1)

instance FromCbor UUID.UUID where
  fromCbor =
    Decoder
      ( Dec.decodeTagCanonical >>= \case
          37 ->
            Dec.decodeBytesCanonical >>= \b ->
              case UUID.fromByteString (LBS.fromStrict b) of
                Just u -> pure u
                Nothing -> fail "Expect uuid bytestring"
          _ -> fail "Expect uuid tag 37"
      )

decodeFieldOf :: (FromCbor v) => Text -> Decoder s v
decodeFieldOf name =
  Decoder Dec.decodeStringCanonical >>= \f ->
    if f == name
      then fromCbor
      else fail ("unexpected field name: " ++ show f)

decodeNormalProd :: Decoder s Int
decodeNormalProd = Decoder Dec.decodeListLenCanonical

decodeNormalProdOf :: Int -> Decoder s ()
decodeNormalProdOf = coerce Dec.decodeListLenCanonicalOf

decodeRecordProd :: Decoder s Int
decodeRecordProd = Decoder Dec.decodeMapLenCanonical

decodeRecordProdOf :: Int -> Decoder s ()
decodeRecordProdOf l =
  Decoder Dec.decodeMapLenCanonical >>= \len ->
    if l == len then pure () else fail "record size mismatch"

data SumKind
  = SkUnit Text
  | SkNormal Text {-# UNPACK #-} Int
  | SkRecord Text {-# UNPACK #-} Int

decodeSum :: Decoder s SumKind
decodeSum =
  Decoder
    ( Dec.peekTokenType >>= \case
        Dec.TypeString -> SkUnit <$> Dec.decodeStringCanonical
        Dec.TypeTag -> do
          Dec.decodeTagCanonical >>= \case
            27 -> pure ()
            _ -> fail "invalid tag, expect tag 27"
          Dec.decodeListLenCanonicalOf 2
          t <- Dec.decodeStringCanonical
          Dec.peekTokenType >>= \case
            Dec.TypeListLen -> SkNormal t <$> Dec.decodeListLenCanonical
            Dec.TypeMapLen -> SkRecord t <$> Dec.decodeMapLenCanonical
            _ -> fail "expect map or list"
        _ -> fail ": expect string or tag 27"
    )
