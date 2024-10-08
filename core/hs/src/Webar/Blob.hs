{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | distinguish object and blob data to
--
--     * avoid collision
--
--     * all data in object store has similar object structure
module Webar.Blob
  ( BlobData,
    isImage,
    BinJsonData (..),
    CborData (..),
    ImageData (..),
    BlobId (..),
  )
where

import Data.ByteString (ByteString)
import Webar.Blob.Internal
import Webar.Bytes (ByteBuffer)
import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Digest (Digest)

newtype BinJsonData t = BinJsonData ByteString
  deriving (ByteBuffer)

instance BlobData (BinJsonData t)

newtype CborData t = CborData ByteString
  deriving (ByteBuffer)

instance BlobData (CborData t)

newtype ImageData = ImageData ByteString
  deriving (ByteBuffer)

instance BlobData ImageData where
  isImage _ = True

newtype BlobId t = BlobId Digest
  deriving (Show, Eq, Ord, FromCbor, ToCbor, FromJSON, ToJSON)
