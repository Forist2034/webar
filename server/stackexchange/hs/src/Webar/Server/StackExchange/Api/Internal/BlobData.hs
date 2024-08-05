{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Blob data is experimental, so definition is only exported here
module Webar.Server.StackExchange.Api.Internal.BlobData where

import qualified Data.Set as S
import Webar.Blob
import Webar.Bytes (ByteBuffer)
import qualified Webar.Server.StackExchange.Api.Model as Api

newtype ApiData t = ApiData (BinJsonData t)
  deriving (ByteBuffer, BlobData)

newtype ListData t = ListData (CborData (S.Set t))
  deriving (ByteBuffer, BlobData)

newtype FilterData = FilterData (BinJsonData Api.Filter)
  deriving (ByteBuffer, BlobData)