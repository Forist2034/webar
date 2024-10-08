{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.RestApi.Filter
  ( FilterData,
    FilterInfo (..),
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
import Webar.Blob
import Webar.Data.TH
import Webar.Object
import Webar.Server.StackExchange.RestApi.Filter.Internal
import Webar.Server.StackExchange.RestApi.Internal.BlobData
import Webar.Server.StackExchange.RestApi.Internal.Version (ApiVersion)

data FilterInfo = FilterInfo
  { fiName :: Text,
    fiApiVersion :: ApiVersion,
    fiSafe :: Bool,
    fiBody :: BlobId FilterData
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
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
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''TypeMap

filterFields :: (ToFilter a) => Proxy a -> Fields
filterFields p = addToFields p (Fields mempty)
