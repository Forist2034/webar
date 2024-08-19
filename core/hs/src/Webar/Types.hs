{-# LANGUAGE TemplateHaskell #-}

module Webar.Types where

import Data.Word (Word32, Word64)
import Webar.Data.TH

data Timestamp = Timestamp
  { tsSecs :: {-# UNPACK #-} !Word64,
    tsNanos :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq, Ord)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Timestamp
