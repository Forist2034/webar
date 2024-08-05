module Webar.Blob.Internal where

import Webar.Bytes (ByteArrayAccess)

-- | provide information about blob data so that store can perform optimization,
--  methods is not stable now
class (ByteArrayAccess t) => BlobData t