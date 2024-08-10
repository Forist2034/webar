module Webar.Blob.Internal where

import Data.Proxy (Proxy)
import Webar.Bytes (ByteBuffer)

-- | provide information about blob data so that store can perform optimization,
--  methods is not stable now
class (ByteBuffer t) => BlobData t where
  isImage :: Proxy t -> Bool
  isImage _ = False