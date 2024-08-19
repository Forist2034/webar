{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Digest
  ( SubDir (..),
    Sha256,
    sha256SubDir,
    sha256ToString,
    Digest (..),
    hashBytes,
    hashHandle,
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
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import System.IO
import qualified Webar.Bytes as Bytes
import Webar.Data.Cbor
import Webar.Data.Json
import Webar.Data.TH

-- TODO: replace cryptonite

-- | sub directory of store path e.g. 1a in sha256-1a/1abcd34567,
-- may use less than 16 bits
data SubDir
  = SubDir
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Word8

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
sha256SubDir (Sha256 s) = SubDir (BA.index s 0) (BA.index s 1)

newtype Digest = DSha256 Sha256
  deriving (Show, Eq, Ord)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . tail
    }
  ''Digest

newtype BufView t = BufView t

instance (Bytes.ByteBuffer t) => BA.ByteArrayAccess (BufView t) where
  length (BufView t) = Bytes.length t
  withByteArray (BufView t) f =
    Bytes.withBuffer t (\(Bytes.Buffer ptr _) -> f (castPtr ptr))

hashBytes :: (Bytes.ByteBuffer ba) => ba -> Digest
hashBytes bs = DSha256 (Sha256 (H.hash (BufView bs)))

hashHandle :: Handle -> IO Digest
hashHandle h = do
  ctx <- H.IO.hashMutableInit
  buf <- newPinnedByteArray bufSize
  doHash ctx buf
  where
    bufSize = 16 * 1024

    doHash ctx buf = do
      l <- withMutableByteArrayContents buf (\ptr -> hGetBuf h ptr bufSize)
      if l == 0
        then DSha256 . Sha256 <$> H.IO.hashMutableFinalize ctx
        else
          withMutableByteArrayContents
            buf
            (\ptr -> H.IO.hashMutableUpdate ctx (BA.MemView ptr l))
            >> doHash ctx buf