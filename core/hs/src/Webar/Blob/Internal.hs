module Webar.Blob.Internal where

import Webar.Bytes (ByteBuffer)

-- | provide information about blob data so that store can perform optimization,
--  methods is not stable now
class (ByteBuffer t) => BlobData t