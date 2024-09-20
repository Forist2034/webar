{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.RestApi.Source
  ( -- * Archive
    Archive (..),

    -- ** node
    NodeLeaf (..),
    NodeChild (..),
    SiteChildNode (..),
    ArchiveNode (..),

    -- ** edge
    EdgeLeaf (..),
    EdgeChild (..),
    EdgeBranch (..),
    AnswerEdge (..),
    CollectiveEdge (..),
    QuestionEdge (..),
    TagEdge (..),
    UserEdge (..),
    SiteChildEdge (..),
    SiteEdge (..),
    ArchiveChildEdge (..),
    ArchiveEdge (..),

    -- * Request
    RequestRecord (..),

    -- ** fetch
    F.FetchInfo (..),
    FetchId,

    -- ** http request
    H.RequestId (..),
    ResponseId (..),
    Request,
    Response,
    HttpInfo (..),

    -- ** api response
    ResponseData (..),
    ApiResponseInfo (..),

    -- * Snapshot
    SnapshotEdge (..),
    SnapshotType (..),
    NodeData,
    NodeContent (..),
    SetData,
    SetContent (..),
    SnapshotInfo (..),
    NodeInfo,
    EdgeInfo,
  )
where

import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Word (Word32)
import Webar.Blob (BlobId)
import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Cbor.TH
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import qualified Webar.Fetch.Http as F
import qualified Webar.Http as H
import Webar.Object (ObjectId)
import Webar.Server.StackExchange.RestApi.Internal.BlobData
import Webar.Server.StackExchange.RestApi.Types
import Webar.Types (Timestamp)

data NodeLeaf = NlNode
  deriving (Show, Eq)

newtype NodeChild c = NcChild c
  deriving (Show, Eq)

data SiteChildNode
  = ScnAnswer AnswerId {-# UNPACK #-} NodeLeaf
  | ScnBadge BadgeId {-# UNPACK #-} NodeLeaf
  | ScnComment CommentId {-# UNPACK #-} NodeLeaf
  | ScnCollective CollectiveSlug {-# UNPACK #-} NodeLeaf
  | ScnInfo {-# UNPACK #-} NodeLeaf
  | ScnQuestion QuestionId {-# UNPACK #-} NodeLeaf
  | ScnRevision RevisionId {-# UNPACK #-} NodeLeaf
  | ScnTag TagName {-# UNPACK #-} NodeLeaf
  | ScnTagWiki TagName {-# UNPACK #-} NodeLeaf
  | ScnTagSynonym TagName {-# UNPACK #-} NodeLeaf
  | ScnUser UserId {-# UNPACK #-} NodeLeaf
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( \(n, c) ->
           deriveSumData
             defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop c}
             n
       )
       [ (''NodeLeaf, 2),
         (''NodeChild, 2),
         (''SiteChildNode, 3)
       ]
 )

data ArchiveNode = AnSite ApiSiteParameter (NodeChild SiteChildNode)
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''ArchiveNode

newtype EdgeLeaf e = ElEdge e
  deriving (Show, Eq)

newtype EdgeChild c = EcChild c
  deriving (Show, Eq)

data EdgeBranch e c = EbEdge e | EbChild c
  deriving (Show, Eq)

data AnswerEdge = AnsComment | AnsRevision
  deriving (Show, Eq)

data CollectiveEdge = ColAnswer | ColQuestion | ColTag | ColUser
  deriving (Show, Eq)

data QuestionEdge = QueAnswer | QueComment | QueRevision
  deriving (Show, Eq)

data TagEdge = TTagSynonym
  deriving (Show, Eq)

data UserEdge = UsrAnswer | UsrBadge | UsrComment | UsrQuestion
  deriving (Show, Eq)

data SiteChildEdge
  = SceAnswer AnswerId (EdgeLeaf AnswerEdge)
  | SceCollective CollectiveSlug (EdgeLeaf CollectiveEdge)
  | SceQuestion QuestionId (EdgeLeaf QuestionEdge)
  | SceTag TagName (EdgeLeaf TagEdge)
  | SceUser UserId (EdgeLeaf UserEdge)
  deriving (Show, Eq)

data SiteEdge = SitBadge | SitTag
  deriving (Show, Eq)

data ArchiveChildEdge = AceSite ApiSiteParameter (EdgeBranch SiteEdge SiteChildEdge)
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( \(n, c) ->
           deriveSumData
             defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop c}
             n
       )
       [ (''EdgeLeaf, 2),
         (''EdgeChild, 2),
         (''EdgeBranch, 2),
         (''AnswerEdge, 3),
         (''CollectiveEdge, 3),
         (''QuestionEdge, 3),
         (''TagEdge, 1),
         (''UserEdge, 3),
         (''SiteChildEdge, 3),
         (''SiteEdge, 3),
         (''ArchiveChildEdge, 3)
       ]
 )

newtype ArchiveEdge = ArchiveEdge (EdgeChild ArchiveChildEdge)
  deriving (Show, Eq, FromCbor, ToCbor, FromJSON, ToJSON)

data Archive = ANode ArchiveNode | AEdge ArchiveEdge
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . tail}
  ''Archive

data RequestRecord = RrFetch | RrHttpRequest | RrApiResponse
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''RequestRecord

type FetchId = F.FetchId ()

newtype ResponseId = RespXRequestGuid UUID
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '-' . drop 4
    }
  ''ResponseId

type Request = H.Request H.RequestId ()

type Response = H.Response (Maybe ResponseId)

data HttpInfo = HttpInfo
  { hiFetch :: FetchId,
    hiSeq :: Word32,
    hiResponseIndex :: Word32,
    hiRequest :: Request,
    hiResponse :: Response (BlobId H.JsonBody)
  }
  deriving (Show)

deriveProdCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpInfo

data ResponseData r
  = RdNode
      { rdnType :: ArchiveNode,
        rdnResponse :: r
      }
  | RdEdge
      { rdeType :: ArchiveEdge,
        rdeFull :: Bool,
        rdeResponses :: Vector r
      }
  | RdRevision
      { rdrType :: Archive,
        rdrResponses :: Vector r
      }
  deriving (Show)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2,
      sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
    }
  ''ResponseData

data ApiResponseInfo = ApiResponseInfo
  { arFetch :: FetchId,
    arSeq :: Word32,
    arApi :: ApiInfo,
    arTimestamp :: Timestamp,
    arResponse :: ResponseData (ObjectId HttpInfo)
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ApiResponseInfo

data SnapshotEdge = SeSet
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SnapshotEdge

data SnapshotType = StNode | StEdge {-# UNPACK #-} SnapshotEdge
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SnapshotType

newtype NodeContent t = NcNormal (BlobId (NodeData t))
  deriving (Show)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''NodeContent

data SetContent t = ScNormal
  { scContent :: BlobId (SetData t),
    scFull :: Bool
  }
  deriving (Show)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2,
      sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
    }
  ''SetContent

data SnapshotInfo c = SnapshotInfo
  { siFetch :: FetchId,
    siApiResponse :: ObjectId ApiResponseInfo,
    siApi :: ApiInfo,
    siTimestamp :: Timestamp,
    siContent :: c
  }
  deriving (Show)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''SnapshotInfo

type NodeInfo t = SnapshotInfo (NodeContent t)

type EdgeInfo t = SnapshotInfo (SetContent t)
