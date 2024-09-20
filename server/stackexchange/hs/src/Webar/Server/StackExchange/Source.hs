{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Source where

import Webar.Data.TH
import Webar.Object
import qualified Webar.Server.StackExchange.Image.Source as Img.Source
import qualified Webar.Server.StackExchange.RestApi.Source as RestApi
import Webar.Types (Server (..), Version (Version))

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
  = RtRestApi {-# UNPACK #-} RestApi.RequestRecord
  | RtFilter
  | RtImageRequest {-# UNPACK #-} Img.Source.RequestRecord
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''RecordType

data ArchiveInfo
  = AiRestApi RestApi.Archive
  | AiImage Img.Source.ArchiveImage
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ArchiveInfo

type ArchiveId = ObjectId ArchiveInfo

data SnapshotType
  = StRestApi RestApi.SnapshotType
  | StImage {-# UNPACK #-} Img.Source.SnapshotType
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SnapshotType