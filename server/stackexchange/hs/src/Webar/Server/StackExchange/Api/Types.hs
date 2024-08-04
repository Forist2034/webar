{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Types
  ( ApiVersion (..),
    module T,
  )
where

import Webar.Data.TH
import Webar.Server.StackExchange.Types as T

data ApiVersion
  = Api2_3
  deriving (Show, Eq, Ord)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = fmap (\c -> if c == '_' then '.' else c) . drop 3
    }
  ''ApiVersion