{-# LANGUAGE NoImplicitPrelude #-}

module Webar.Bytes
  ( Buffer (..),
    ByteBuffer,
    length,
    withBuffer,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.U
import Data.Int (Int)
import Data.Word (Word8)
import Foreign.Ptr
import Prelude (IO)

data Buffer = Buffer {-# UNPACK #-} (Ptr Word8) {-# UNPACK #-} Int

class ByteBuffer a where
  length :: a -> Int
  withBuffer :: a -> (Buffer -> IO b) -> IO b

instance ByteBuffer BS.ByteString where
  length = BS.length
  withBuffer bs f =
    BS.U.unsafeUseAsCString
      bs
      (\ptr -> f (Buffer (castPtr ptr) (BS.length bs)))