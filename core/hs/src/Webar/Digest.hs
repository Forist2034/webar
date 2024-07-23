{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Digest
  ( Sha256,
    sha256Hash,
    sha256SubDir,
    sha256ToString,
    sha256Handle,
    Digest (..),
  )
where

import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Encoding as Cbor
import qualified Crypto.Hash as H
import qualified Crypto.Hash.IO as H.IO
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.Aeson.Types as AE
import Data.ByteArray (Bytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.Coerce (coerce)
import Data.Primitive.ByteArray
import qualified Data.Text.Encoding as TE
import System.IO
import Webar.Data.Cbor
import Webar.Data.Json
import Webar.Data.TH
import Webar.Types (SubDir (..))

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

sha256ToString :: Sha256 -> String
sha256ToString (Sha256 s) = show s

sha256SubDir :: Sha256 -> SubDir
sha256SubDir (Sha256 s) =
  SubDir (BA.index s 0) (BA.index s 1) (BA.index s 2) (BA.index s 3)

sha256Handle :: Handle -> IO Sha256
sha256Handle h = do
  ctx <- H.IO.hashMutableInit
  buf <- newPinnedByteArray bufSize
  hashHandle ctx buf
  where
    bufSize = 1024 * 1024 * 64

    hashHandle ctx buf = do
      l <- withMutableByteArrayContents buf (\ptr -> hGetBuf h ptr bufSize)
      if l == 0
        then Sha256 <$> H.IO.hashMutableFinalize ctx
        else
          withMutableByteArrayContents
            buf
            (\ptr -> H.IO.hashMutableUpdate ctx (BA.MemView ptr l))
            >> hashHandle ctx buf

newtype Digest = DSha256 Sha256
  deriving (Show, Eq, Ord)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Digest