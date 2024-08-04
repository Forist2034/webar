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

data ObjectStore h a o = ObjectStore
  { storeServer :: !Server,
    storeHost :: !h,
    baseStore :: !OS.ObjectStore,
    upperStore :: !OS.ObjectStore
  }

openByFilePath :: Server -> h -> OS.ObjectStore -> FilePath -> IO (ObjectStore h a o)
openByFilePath server host base fp =
  ObjectStore server host base <$> OS.openByFilePath fp

addObject ::
  (ToCbor h, ToCbor so, ToCbor o) =>
  ObjectStore h a so ->
  ObjectType a so ->
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

close :: ObjectStore h a o -> IO ()
close s = OS.close (upperStore s)