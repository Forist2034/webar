module Webar.Store.Blob.WithShared
  ( Base.BlobHandle (..),
    BlobStore,
    openByFilePath,
    addBlob,
    close,
  )
where

import Webar.Blob (BlobData)
import qualified Webar.Store.Blob.Base as Base

data BlobStore = BlobStore
  { baseStore :: !Base.BlobStore,
    upperStore :: !Base.BlobStore
  }

openByFilePath :: Base.BlobStore -> FilePath -> IO BlobStore
openByFilePath base fp = BlobStore base <$> Base.openByFilePath fp

addBlob :: (BlobData t) => BlobStore -> t -> IO (Base.BlobHandle t)
addBlob s cont =
  Base.addBlob (baseStore s) cont >>= \h ->
    h <$ Base.linkHandle (upperStore s) (baseStore s) h

close :: BlobStore -> IO ()
close s = Base.close (upperStore s)