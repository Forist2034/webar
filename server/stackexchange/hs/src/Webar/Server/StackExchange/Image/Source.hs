{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Image.Source
  ( ArchiveImage (..),
    module Src,
  )
where

import Data.Text (Text)
import Webar.Data.TH
import Webar.Image.Source as Src

newtype ArchiveImage
  = -- | i.sstatic.net image
    AImgContent Text
  deriving (Show, Eq, Ord)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 4
    }
  ''ArchiveImage