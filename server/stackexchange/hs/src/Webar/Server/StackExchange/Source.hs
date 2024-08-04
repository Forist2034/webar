{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Source where

import Webar.Data.TH
import Webar.Object
import qualified Webar.Server.StackExchange.Api.Request as Api.Request
import qualified Webar.Server.StackExchange.Api.Source as Api.Source
import Webar.Server.StackExchange.Types (ApiSiteParameter)

server :: Server
server = Server {serverName = "StackExchange", serverVersion = 1}

data RecordType
  = RtApiRequest {-# UNPACK #-} Api.Request.RequestRecord
  | RtFilter
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''RecordType

data ArchiveInfo
  = AiSiteApi ApiSiteParameter Api.Source.ArchiveSiteData
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ArchiveInfo

type ArchiveId = ObjectId ArchiveInfo

newtype SnapshotType
  = StApi Api.Source.ApiSnapshotType
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SnapshotType