{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Filter
  ( FilterInfo (..),
    FilterId,
    FilterSpec (..),
    TypeMap (..),
    Fields,
    ToFilter,
    filterFields,
  )
where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Webar.Data.TH
import Webar.Object
import Webar.Server.StackExchange.Api.Filter.Internal
import Webar.Server.StackExchange.Api.Types (ApiVersion)

data FilterInfo = FilterInfo
  { fiName :: Text,
    fiApiVersion :: ApiVersion,
    fiSafe :: Bool,
    fiBody :: DataId
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FilterInfo

type FilterId = ObjectId FilterInfo

data FilterSpec = FilterSpec
  { fsName :: Text,
    fsId :: FilterId
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

filterFields :: (ToFilter a) => Proxy a -> Fields
filterFields p = addToFields p (Fields mempty)
