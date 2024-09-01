{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Webar.Server.Wordpress.RestApi.Internal.BlobData where

import qualified Data.Set as S
import Webar.Blob (BinJsonData, BlobData, CborData)
import Webar.Bytes (ByteBuffer)

newtype NodeData t = NodeData (BinJsonData t)
  deriving (ByteBuffer, BlobData)

newtype SetData t = SetData (CborData (S.Set t))
  deriving (ByteBuffer, BlobData)