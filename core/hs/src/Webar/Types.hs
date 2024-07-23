{-# LANGUAGE TemplateHaskell #-}

module Webar.Types where

import Data.Word (Word32, Word64, Word8)
import Webar.Data.TH

data Timestamp = Timestamp
  { tsSecs :: {-# UNPACK #-} !Word64,
    tsNanos :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq, Ord)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Timestamp

-- | sub directory of store path e.g. 1a in type/1a/1abcd34567,
-- may use less than 32 bits
data SubDir
  = SubDir
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Word8
      {-# UNPACK #-} !Word8