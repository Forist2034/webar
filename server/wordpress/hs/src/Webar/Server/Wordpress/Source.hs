{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.Source where

import Webar.Data.TH
import qualified Webar.Server.Wordpress.RestApi.Source as RestApi
import Webar.Types

server :: Server
server = Server {serverName = "WordPress", serverVersion = Version 1 0}

newtype Archive
  = ARestApi RestApi.Archive
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . tail}
  ''Archive

data FetchType = FtRestApi
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''FetchType

newtype RecordType
  = RtRestApi RestApi.RequestRecord
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''RecordType

newtype SnapshotType
  = StRestApi RestApi.SnapshotType
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SnapshotType