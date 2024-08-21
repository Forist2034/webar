{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Source where

import Webar.Data.TH
import Webar.Object
import qualified Webar.Server.StackExchange.Api.Request as Api.Request
import qualified Webar.Server.StackExchange.Api.Source as Api.Source
import qualified Webar.Server.StackExchange.Image.Source as Img.Source
import Webar.Server.StackExchange.Types (ApiSiteParameter)
import Webar.Types (Version (Version))

server :: Server
server = Server {serverName = "StackExchange", serverVersion = Version 1 0}

data FetchType
  = FtRestApi
  | FtImage
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''FetchType

data RecordType
  = RtApiRequest {-# UNPACK #-} Api.Request.RequestRecord
  | RtFilter
  | RtImageRequest {-# UNPACK #-} Img.Source.RequestRecord
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''RecordType

data ArchiveInfo
  = AiSiteApi ApiSiteParameter Api.Source.ArchiveSiteData
  | AiImage Img.Source.ArchiveImage
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ArchiveInfo

type ArchiveId = ObjectId ArchiveInfo

data SnapshotType
  = StApi Api.Source.ApiSnapshotType
  | StImage {-# UNPACK #-} Img.Source.SnapshotType
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SnapshotType