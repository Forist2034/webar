{-# LANGUAGE TemplateHaskell #-}

module Webar.Fetch where

import Webar.Data.Cbor.TH
import Webar.Types (Server, Version)

-- | fetch metadata
--
--     * support upgrading of data format
--
--     * fetched can be imported without manually specifying server instance
--
--     * detect errors like incompatible version or type mismatch
data FetchMeta i t d = FetchMeta
  { fmServer :: Server,
    fmInstance :: i,
    fmType :: t,
    fmVersion :: Version,
    fmData :: d
  }
  deriving (Show)

deriveProdCbor
  defaultProductOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2,
      sortFields = False
    }
  ''FetchMeta