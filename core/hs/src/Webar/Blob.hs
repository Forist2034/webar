{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | distinguish object and blob data to
--
--     * avoid collision
--
--     * all data in object store has similar object structure
module Webar.Blob
  ( BlobData,
    BinJsonData (..),
    CborData (..),
    BlobId (..),
  )
where

import Data.ByteString (ByteString)
import Webar.Blob.Internal
import Webar.Bytes (ByteArrayAccess)
import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Digest (Digest)

newtype BinJsonData t = BinJsonData ByteString
  deriving (ByteArrayAccess)

instance BlobData (BinJsonData t)

newtype CborData t = CborData ByteString
  deriving (ByteArrayAccess)

instance BlobData (CborData t)

newtype BlobId t = BlobId Digest
  deriving (Show, Eq, Ord, FromCbor, ToCbor, FromJSON, ToJSON)
