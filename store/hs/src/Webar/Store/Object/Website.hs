module Webar.Store.Object.Website
  ( OS.ObjectHandle (..),
    ObjectStore,
    openByFilePath,
    addObject,
    close,
  )
where

import Webar.Data.Cbor (ToCbor)
import Webar.Object
import qualified Webar.Store.Object.Base as OS
import Webar.Types (Server, Version)

data ObjectStore h a st rt = ObjectStore
  { storeServer :: !Server,
    storeInstance :: !h,
    baseStore :: !OS.ObjectStore,
    upperStore :: !OS.ObjectStore
  }

openByFilePath :: Server -> h -> OS.ObjectStore -> FilePath -> IO (ObjectStore h a st rt)
openByFilePath server inst base fp =
  ObjectStore server inst base <$> OS.openByFilePath fp

addObject ::
  (ToCbor h, ToCbor st, ToCbor rt, ToCbor o) =>
  ObjectStore h a st rt ->
  ObjectType a st rt ->
  Version ->
  o ->
  IO (OS.ObjectHandle o)
addObject s ot ver o =
  OS.addObject
    (baseStore s)
    (storeServer s)
    ObjectInfo
      { oiInstance = storeInstance s,
        oiType = ot,
        oiVersion = ver
      }
    o
    >>= \h ->
      h <$ OS.linkHandle (upperStore s) (baseStore s) h

close :: ObjectStore h a st rt -> IO ()
close s = OS.close (upperStore s)