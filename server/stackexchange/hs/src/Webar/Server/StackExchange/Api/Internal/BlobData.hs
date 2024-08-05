{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Blob data is experimental, so definition is only exported here
module Webar.Server.StackExchange.Api.Internal.BlobData where

import qualified Data.Set as S
import Webar.Blob
import Webar.Bytes (ByteArrayAccess)
import qualified Webar.Server.StackExchange.Api.Model as Api

newtype ApiData t = ApiData (BinJsonData t)
  deriving (ByteArrayAccess, BlobData)

newtype ListData t = ListData (CborData (S.Set t))
  deriving (ByteArrayAccess, BlobData)

newtype FilterData = FilterData (BinJsonData Api.Filter)
  deriving (ByteArrayAccess, BlobData)