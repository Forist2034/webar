{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.RestApi.Types where

import Webar.Data.TH

data ApiVersion = Api2
  deriving (Show, Eq, Ord)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 3}
  ''ApiVersion

newtype ApiInfo = ApiInfo {aiVersion :: ApiVersion}
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiInfo