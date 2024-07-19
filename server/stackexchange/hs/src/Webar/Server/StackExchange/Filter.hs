{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Filter where

import Data.Aeson hiding (Options (..))
import Data.Text (Text)
import Webar.Data.TH
import Webar.Server.StackExchange.Types

data FilterInfo = FilterInfo
  { fiName :: Text,
    fiId :: FilterId
  }
  deriving (Show)

data TypeMap a = TypeMap
  { tmAnswer,
    tmBadge,
    tmComment,
    tmCollective,
    tmInfo,
    tmQuestion,
    tmRevision,
    tmTag,
    tmTagSynonym,
    tmTagWiki,
    tmUser ::
      a
  }
  deriving (Show, Functor, Foldable, Traversable)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''TypeMap
