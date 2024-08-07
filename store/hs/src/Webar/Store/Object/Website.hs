module Webar.Store.Object.Website
  ( OS.ObjectHandle (..),
    ObjectStore,
    openByFilePath,
    addObject,
    close,
  )
where

import Data.Word (Word8)
import Webar.Data.Cbor (ToCbor)
import Webar.Object
import qualified Webar.Store.Object.Base as OS

data ObjectStore h a st rt = ObjectStore
  { storeServer :: !Server,
    storeHost :: !h,
    baseStore :: !OS.ObjectStore,
    upperStore :: !OS.ObjectStore
  }

openByFilePath :: Server -> h -> OS.ObjectStore -> FilePath -> IO (ObjectStore h a st rt)
openByFilePath server host base fp =
  ObjectStore server host base <$> OS.openByFilePath fp

addObject ::
  (ToCbor h, ToCbor st, ToCbor rt, ToCbor o) =>
  ObjectStore h a st rt ->
  ObjectType a st rt ->
  Word8 ->
  o ->
  IO (OS.ObjectHandle o)
addObject s ot ver o =
  OS.addObject
    (baseStore s)
    (storeServer s)
    ObjectInfo
      { oiHost = storeHost s,
        oiType = ot,
        oiVersion = ver
      }
    o
    >>= \h ->
      h <$ OS.linkHandle (upperStore s) (baseStore s) h

close :: ObjectStore h a st rt -> IO ()
close s = OS.close (upperStore s)