{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Digest
  ( Sha256 (..),
    sha256Hash,
    Digest (..),
  )
where

import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Encoding as Cbor
import qualified Crypto.Hash as H
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.Aeson.Types as AE
import Data.ByteArray (Bytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.Coerce (coerce)
import qualified Data.Text.Encoding as TE
import Webar.Data.Cbor
import Webar.Data.Json
import Webar.Data.TH

digestToJson :: H.Digest a -> AE.Encoding
digestToJson d = AE.text (TE.decodeASCII (BAE.convertToBase BAE.Base16 d))

digestFromJson :: (H.HashAlgorithm a) => String -> AE.Value -> AE.Parser (H.Digest a)
digestFromJson name =
  AE.withText
    name
    ( \t -> do
        b <- case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 t) of
          Right v -> pure (v :: Bytes)
          Left e -> fail e
        case H.digestFromByteString b of
          Just d -> pure d
          Nothing -> fail "invalid digest"
    )

digestToCbor :: H.Digest a -> Cbor.Encoding
digestToCbor d = Cbor.encodeBytes (BA.convert d)

digestFromCbor :: (H.HashAlgorithm a) => String -> Cbor.Decoder s (H.Digest a)
digestFromCbor msg =
  Cbor.decodeBytesCanonical >>= \bs ->
    case H.digestFromByteString bs of
      Just d -> pure d
      Nothing -> fail msg

newtype Sha256 = Sha256 (H.Digest H.SHA256)
  deriving (Show, Eq, Ord)

sha256Hash :: (BA.ByteArrayAccess ba) => ba -> Sha256
sha256Hash ba = Sha256 (H.hash ba)

instance ToJSON Sha256 where
  toJson (Sha256 d) = digestToJson d

instance FromJSON Sha256 where
  parseJSON = coerce (digestFromJson @H.SHA256 "sha256")

instance ToCbor Sha256 where
  toCbor (Sha256 d) = digestToCbor d

instance FromCbor Sha256 where
  fromCbor = coerce (digestFromCbor @H.SHA256 "invalid sha256 digest")

newtype Digest = DSha256 Sha256
  deriving (Show, Eq, Ord)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Digest