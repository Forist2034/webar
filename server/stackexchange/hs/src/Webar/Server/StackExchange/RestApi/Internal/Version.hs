{-# LANGUAGE TemplateHaskell #-}

-- | avoid cyclic dependencies between Filter and Types
module Webar.Server.StackExchange.RestApi.Internal.Version (ApiVersion (..)) where

import Webar.Data.TH

data ApiVersion
  = Api2_3
  deriving (Show, Eq, Ord)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = fmap (\c -> if c == '_' then '.' else c) . drop 3
    }
  ''ApiVersion