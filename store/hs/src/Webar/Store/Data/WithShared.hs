module Webar.Store.Data.WithShared
  ( DS.DataHandle (..),
    DataStore,
    openByFilePath,
    addByteString,
    addFileByRawPathAt,
    close,
  )
where

import Data.ByteString (ByteString)
import System.Posix.ByteString (RawFilePath)
import System.Posix.Types (Fd)
import qualified Webar.Store.Data.Base as DS

data DataStore = DataStore
  { baseStore :: !DS.DataStore,
    upperStore :: !DS.DataStore
  }

openByFilePath :: DS.DataStore -> FilePath -> IO DataStore
openByFilePath base fp = DataStore base <$> DS.openByFilePath fp

addByteString :: DataStore -> ByteString -> IO DS.DataHandle
addByteString s cont =
  DS.addByteString (baseStore s) cont >>= \h ->
    h <$ DS.linkHandle (upperStore s) (baseStore s) h

addFileByRawPathAt :: DataStore -> Fd -> RawFilePath -> IO DS.DataHandle
addFileByRawPathAt s dirFd fp =
  DS.addFileByRawPathAt (baseStore s) dirFd fp >>= \h ->
    h <$ DS.linkHandle (upperStore s) (baseStore s) h

close :: DataStore -> IO ()
close s = DS.close (upperStore s)