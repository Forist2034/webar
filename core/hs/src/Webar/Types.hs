{-# LANGUAGE TemplateHaskell #-}

module Webar.Types where

import Data.Word (Word32, Word64)
import Webar.Data.TH

data Timestamp = Timestamp
  { tsSeconds :: {-# UNPACK #-} !Word64,
    tsNanoseconds :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq, Ord)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''Timestamp
