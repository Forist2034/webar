{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Data.BinJson
  ( BinValue,
    parseBinValue,
    WithBinValue (..),
  )
where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Enc
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Int (Int64)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import Webar.Data.Cbor (FromCbor (..), ToCbor (..))
import qualified Webar.Data.Json as Json

data Field = !K.Key := !BinValue
  deriving (Show, Eq)

data Number
  = NInt {-# UNPACK #-} !Int64
  | NWord {-# UNPACK #-} !Word64
  | NScientific !Integer !Int
  deriving (Show, Eq)

data BinValue
  = JString !Text
  | JBool {-# UNPACK #-} !Bool
  | JNull
  | JNumber {-# UNPACK #-} !Number
  | JArray !(V.Vector BinValue)
  | JObject !(V.Vector Field)
  deriving (Show, Eq)

instance ToCbor BinValue where
  toCbor (JString s) = encodeString s
  toCbor (JBool b) = encodeBool b
  toCbor JNull = encodeNull
  toCbor (JNumber n) = case n of
    NInt i -> encodeInt64 i
    NWord w -> encodeWord64 w
    NScientific v e ->
      encodeTag 4
        <> encodeListLen 2
        <> (encodeInt e <> encodeInteger v)
  toCbor (JArray v) =
    encodeListLen (fromIntegral (V.length v))
      <> V.foldMap' toCbor v
  toCbor (JObject fs) =
    encodeMapLen (fromIntegral (V.length fs))
      <> V.foldMap'
        (\(k := v) -> encodeString (K.toText k) <> toCbor v)
        fs

instance FromCbor BinValue where
  fromCbor =
    peekTokenType >>= \case
      TypeString -> JString <$> decodeStringCanonical
      TypeBool -> JBool <$> decodeBool
      TypeNull -> JNull <$ decodeNull
      TypeUInt -> decodeWordNum
      TypeUInt64 -> decodeWordNum
      TypeNInt -> decodeIntNum
      TypeNInt64 -> decodeIntNum
      TypeListLen -> JArray <$> fromCbor
      TypeMapLen ->
        decodeMapLenCanonical >>= \l ->
          JObject . V.fromListN l <$> goObj l
      TypeTag ->
        decodeTagCanonical >>= \case
          4 ->
            decodeListLenCanonicalOf 2 >> do
              e <- decodeInt
              m <- decodeInteger
              pure (JNumber (NScientific m e))
          _ -> fail "invalid tag"
      _ -> fail "invalid type"
    where
      decodeIntNum = JNumber . NInt <$> decodeInt64Canonical
      decodeWordNum = JNumber . NWord <$> decodeWord64Canonical
      goObj 0 = pure []
      goObj n = do
        k <- decodeStringCanonical
        v <- fromCbor
        r <- goObj (n - 1)
        pure (K.fromText k := v : r)

encodeJson :: BinValue -> Enc.Encoding
encodeJson (JString t) = Enc.text t
encodeJson (JBool b) = Enc.bool b
encodeJson JNull = Enc.null_
encodeJson (JNumber n) = case n of
  NInt i -> Enc.int64 i
  NWord w -> Enc.word64 w
  NScientific m e -> Enc.scientific (Sci.scientific m e)
encodeJson (JArray a) =
  Enc.list encodeJson (V.toList a)
encodeJson (JObject fs) =
  Enc.pairs (V.foldMap' (\(k := v) -> Enc.pair k (encodeJson v)) fs)

instance Json.ToJSON BinValue where
  toJson = encodeJson

instance Aeson.ToJSON BinValue where
  toJSON (JString t) = Aeson.String t
  toJSON (JBool b) = Aeson.Bool b
  toJSON JNull = Aeson.Null
  toJSON (JNumber n) =
    Aeson.Number
      ( case n of
          NInt i -> fromIntegral i
          NWord w -> fromIntegral w
          NScientific m e -> Sci.scientific m e
      )
  toJSON (JArray a) = Aeson.Array (V.map Aeson.toJSON a)
  toJSON (JObject fs) =
    Aeson.Object
      ( V.foldl'
          (\lm (k := v) -> KM.insert k (Aeson.toJSON v) lm)
          KM.empty
          fs
      )

  toEncoding = encodeJson

instance Aeson.FromJSON BinValue where
  parseJSON (Aeson.String s) = pure (JString s)
  parseJSON (Aeson.Bool b) = pure (JBool b)
  parseJSON Aeson.Null = pure JNull
  parseJSON (Aeson.Number sci)
    | Just i <- Sci.toBoundedInteger @Int64 sci =
        pure (JNumber (NInt i))
    | Just i <- Sci.toBoundedInteger @Word64 sci =
        pure (JNumber (NWord i))
    | Sci.isFloating sci =
        let norm = Sci.normalize sci
         in pure
              ( JNumber
                  ( NScientific
                      (Sci.coefficient norm)
                      (Sci.base10Exponent norm)
                  )
              )
    | otherwise = fail "integer greater than 64 bit is not supported"
  parseJSON (Aeson.Array a) =
    JArray
      <$> V.imapM
        (\i v -> Aeson.parseJSON v Aeson.<?> Aeson.Index i)
        a
  parseJSON (Aeson.Object o) =
    JObject . V.fromList
      <$> traverse
        (\(k, v) -> fmap (\rv -> k := rv) (Aeson.parseJSON v Aeson.<?> Aeson.Key k))
        (KM.toAscList o)

parseBinValue :: (Aeson.FromJSON a) => BinValue -> Either String a
parseBinValue bv = Aeson.parseEither Aeson.parseJSON (Aeson.toJSON bv)

data WithBinValue a = WithBinValue !BinValue !a

instance (Aeson.FromJSON a) => Aeson.FromJSON (WithBinValue a) where
  parseJSON v = liftA2 WithBinValue (Aeson.parseJSON v) (Aeson.parseJSON v)