{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Source
  ( -- * Archive info
    ArchiveAnswer (..),
    ArchiveBadge (..),
    ArchiveComment (..),
    ArchiveCollective (..),
    ArchiveRevision (..),
    ArchiveQuestion (..),
    ArchiveTag (..),
    ArchiveTagWiki (..),
    ArchiveTagSynonym (..),
    ArchiveUser (..),
    ArchiveSiteData (..),

    -- * Snapshot info
    ApiSnapshotType (..),

    -- ** api object
    ApiData,
    Content (..),
    ObjectMeta (..),

    -- ** item list
    ListData,
    ListContent (..),
    ListMeta (..),
  )
where

import Webar.Blob (BlobId)
import Webar.Data.TH
import Webar.Server.StackExchange.Api.Filter (FilterId)
import Webar.Server.StackExchange.Api.Internal.BlobData
import Webar.Server.StackExchange.Api.Request
  ( ApiResponseId,
    FetchId,
  )
import Webar.Server.StackExchange.Api.Types
import Webar.Types (Timestamp)

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
           defaultSumOptions
             { constructorTagModifier = camelTo2 '_' . drop 4
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
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 3
    }
  ''ArchiveSiteData

data ApiSnapshotType = AstObject | AstList
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 3
    }
  ''ApiSnapshotType

newtype Content t
  = CNormal (BlobId (ApiData t))
  deriving (Show)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . tail
    }
  ''Content

data ObjectMeta t = ObjectMeta
  { objFetch :: FetchId,
    objApiResponse :: ApiResponseId,
    objContent :: Content t,
    objApiVersion :: ApiVersion,
    objFilter :: FilterId,
    objTimestamp :: Timestamp
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ObjectMeta

data ListContent t = LcNormal
  { lcContent :: BlobId (ListData t),
    lcFull :: Bool
  }
  deriving (Show)

deriveSumData
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ListContent

data ListMeta t = ListMeta
  { listFetch :: FetchId,
    listApiResponse :: ApiResponseId,
    listContent :: ListContent t,
    listApiVersion :: ApiVersion,
    listTimestamp :: Timestamp
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''ListMeta