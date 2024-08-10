{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Http
  ( StatusCode,
    Method,
    HeaderMap,
    JsonBody (..),
    Request (..),
    Response (..),
  )
where

import qualified Codec.CBOR.ByteArray as BA
import qualified Codec.CBOR.ByteArray.Sliced as SBA
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word16)
import Webar.Blob.Internal (BlobData)
import Webar.Bytes (ByteBuffer)
import Webar.Data.Cbor (FromCbor (..), ToCbor (..))
import Webar.Data.Cbor.TH
import Webar.Types (Timestamp)

newtype StatusCode = StatusCode Word16
  deriving (Show, Eq, FromCbor, ToCbor)

newtype Method = Method {methodText :: Text}
  deriving (Show, Eq, FromCbor, ToCbor)

newtype HeaderName = HeaderName {headerNameText :: Text}
  deriving (Show, Eq, Ord, FromCbor, ToCbor)

data HeaderValue
  = HvText {-# UNPACK #-} Text
  | HvBinary {-# UNPACK #-} ShortByteString
  deriving (Show, Eq)

instance ToCbor HeaderValue where
  toCbor (HvText t) = encodeString t
  toCbor (HvBinary b) = encodeByteArray (SBA.fromShortByteString b)

instance FromCbor HeaderValue where
  fromCbor =
    peekTokenType >>= \case
      TypeString -> HvText <$> decodeStringCanonical
      TypeBytes -> HvBinary . BA.toShortByteString <$> decodeByteArrayCanonical
      _ -> fail "invalid type for header value"

data Header
  = Header
      {-# UNPACK #-} HeaderName
      {-# UNPACK #-} (V.Vector HeaderValue)
  deriving (Show, Eq)

newtype HeaderMap = HeaderMap (V.Vector Header)
  deriving (Show, Eq)

instance ToCbor HeaderMap where
  toCbor (HeaderMap mp) =
    encodeMapLen (fromIntegral (V.length mp))
      <> V.foldMap' (\(Header n v) -> toCbor n <> toCbor v) mp

instance FromCbor HeaderMap where
  fromCbor =
    decodeMapLenCanonical
      >>= \l -> HeaderMap . V.fromListN l <$> go l
    where
      go 0 = pure []
      go n = do
        name <- fromCbor
        values <- fromCbor
        r <- go (n - 1)
        pure (Header name values : r)

newtype JsonBody = JsonBody {jsonBody :: ByteString}
  deriving (Show, ByteBuffer, FromCbor)

instance BlobData JsonBody

data Request i b = Request
  { reqId :: i,
    reqMethod :: Method,
    reqUrl :: Text,
    reqTimestamp :: Timestamp,
    reqBody :: b
  }
  deriving (Show)

deriveProdCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Request

data Response i b = Response
  { respId :: i,
    respTimestamp :: Timestamp,
    respStatus :: StatusCode,
    respHeaders :: HeaderMap,
    respBody :: b
  }
  deriving (Show)

deriveProdCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''Response