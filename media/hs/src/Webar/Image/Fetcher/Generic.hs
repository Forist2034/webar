{-# LANGUAGE TemplateHaskell #-}

module Webar.Image.Fetcher.Generic where

import Data.Text (Text)
import qualified Data.Vector as V
import Webar.Data.Cbor.TH

data ImageSpec i = ImageSpec
  { imgId :: i,
    imgPreferredUrl :: Text,
    imgOtherUrls :: V.Vector Text
  }
  deriving (Show)

deriveProdToCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ImageSpec
