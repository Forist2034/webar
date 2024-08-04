{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Source where

import Data.Word (Word32)
import Webar.Data.TH
import Webar.Object
import Webar.Server.StackExchange.Api.Filter (FilterId)
import Webar.Server.StackExchange.Api.Request
  ( ApiResponseId,
    FetchId,
  )
import Webar.Server.StackExchange.Api.Types
import Webar.Types (Timestamp)

server :: Server
server = Server {serverName = "StackExchange-api", serverVersion = 1}

data ArchiveAnswer
  = AAnsInfo
  | AAnsComment
  | AAnsRevision
  deriving (Show, Eq)

data ArchiveBadge = ABdgInfo
  deriving (Show, Eq)

data ArchiveComment = AComInfo
  deriving (Show, Eq)

data ArchiveCollective
  = AColInfo
  | AColAnswer
  | AColQuestion
  | AColTag
  | AColUser
  deriving (Show, Eq)

data ArchiveRevision = ARevInfo
  deriving (Show, Eq)

data ArchiveQuestion = AQueInfo | AQueAnswer | AQueComment | AQueRevision
  deriving (Show, Eq)

data ArchiveTag = ATagInfo | ATagSynonym
  deriving (Show, Eq)

data ArchiveTagWiki = ATWkInfo
  deriving (Show, Eq)

data ArchiveTagSynonym = ATSynInfo
  deriving (Show, Eq)

data ArchiveUser = AUsrInfo | AUsrAnswer | AUsrBadge | AUsrComment | AUsrQuestion
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( deriveSumData
           SumOptions
             { sumProduct = ProductOptions {fieldLabelModifier = id},
               constructorTagModifier = camelTo2 '_' . drop 4
             }
       )
       [ ''ArchiveAnswer,
         ''ArchiveBadge,
         ''ArchiveComment,
         ''ArchiveCollective,
         ''ArchiveRevision,
         ''ArchiveQuestion,
         ''ArchiveTag,
         ''ArchiveTagWiki,
         ''ArchiveTagSynonym,
         ''ArchiveUser
       ]
 )

data ArchiveSiteData
  = AsdAnswer AnswerId ArchiveAnswer
  | AsdBadge BadgeId {-# UNPACK #-} ArchiveBadge
  | AsdComment CommentId {-# UNPACK #-} ArchiveComment
  | AsdCollective CollectiveSlug ArchiveCollective
  | AsdInfo
  | AsdRevision RevisionId {-# UNPACK #-} ArchiveRevision
  | AsdQuestion QuestionId ArchiveQuestion
  | AsdTag TagName ArchiveTag
  | AsdTagWiki TagName {-# UNPACK #-} ArchiveTagWiki
  | AsdTagSynonym TagName {-# UNPACK #-} ArchiveTagSynonym
  | AsdUser UserId ArchiveUser
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 3
    }
  ''ArchiveSiteData

data ArchiveInfo
  = AiSite ApiSiteParameter ArchiveSiteData
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ArchiveInfo

type ArchiveId = ObjectId ArchiveInfo

data RecordType
  = RtFetch
  | RtHttpRequest
  | RtApiResponse
  | RtFilter
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''RecordType

newtype Content
  = CNormal DataId
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Content

data Metadata = Metadata
  { metaFetch :: FetchId,
    metaApiResponse :: ApiResponseId,
    metaApiIndex :: Maybe Word32,
    metaContent :: Content,
    metaApiVersion :: ApiVersion,
    metaFilter :: FilterId,
    metaTimestamp :: Timestamp
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''Metadata

data ListContent = LcNormal
  { lcContent :: DataId,
    lcFull :: Bool
  }
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ListContent

data ListMeta = ListMeta
  { listFetch :: FetchId,
    listApiResponse :: ApiResponseId,
    listContent :: ListContent,
    listApiVersion :: ApiVersion,
    listTimestamp :: Timestamp
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''ListMeta