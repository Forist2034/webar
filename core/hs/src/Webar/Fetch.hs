{-# LANGUAGE TemplateHaskell #-}

module Webar.Fetch where

import Data.Text (Text)
import Data.Word (Word8)
import Webar.Data.TH

-- | fetch metadata
--
--     * support upgrading of data format
--
--     * fetched can be imported without manually specifying server instance
--
--     * detect errors like incompatible version or type mismatch
data FetchMeta i t d = FetchMeta
  { fmServer :: Text,
    fmInstance :: i,
    fmType :: t,
    fmVersion :: Word8,
    fmData :: d
  }
  deriving (Show)

deriveProdData
  defaultProductOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2,
      sortFields = False
    }
  ''FetchMeta