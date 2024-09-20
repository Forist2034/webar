{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Blob data is experimental, so definition is only exported here
module Webar.Server.StackExchange.RestApi.Internal.BlobData where

import qualified Data.Set as S
import Webar.Blob
import Webar.Bytes (ByteBuffer)
import qualified Webar.Server.StackExchange.RestApi.Model as Api

newtype NodeData t = NodeData (BinJsonData t)
  deriving (ByteBuffer, BlobData)

newtype SetData t = SetData (CborData (S.Set t))
  deriving (ByteBuffer, BlobData)

newtype FilterData = FilterData (BinJsonData Api.Filter)
  deriving (ByteBuffer, BlobData)