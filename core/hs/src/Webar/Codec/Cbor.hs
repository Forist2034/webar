module Webar.Codec.Cbor
  ( ToCbor (..),
    FromCbor (..),
    -- only export basic functions now
    encodeStrictBs,
    decodeStrictBsThrow,
  )
where

import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Exception (Exception, throw)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Webar.Codec.Cbor.Decoding
import Webar.Codec.Cbor.Encoding

encodeStrictBs :: (ToCbor a) => a -> BS.ByteString
encodeStrictBs = toStrictByteString . toCbor

data DecodeError
  = DeserializeError DeserialiseFailure
  | RemainingInputs LBS.ByteString
  deriving (Show)

instance Exception DecodeError

fromResult :: Either DeserialiseFailure (LBS.ByteString, a) -> Either DecodeError a
fromResult (Right (bs, v))
  | LBS.null bs = Right v
  | otherwise = Left (RemainingInputs bs)
fromResult (Left e) = Left (DeserializeError e)

decodeLazyBs :: (FromCbor a) => LBS.ByteString -> Either DecodeError a
decodeLazyBs bs = fromResult (deserialiseFromBytes fromCbor bs)

decodeStrictBs :: (FromCbor a) => BS.ByteString -> Either DecodeError a
decodeStrictBs = decodeLazyBs . LBS.fromStrict

decodeStrictBsThrow :: (FromCbor a) => BS.ByteString -> a
decodeStrictBsThrow bs = case decodeStrictBs bs of
  Right r -> r
  Left e -> throw e
