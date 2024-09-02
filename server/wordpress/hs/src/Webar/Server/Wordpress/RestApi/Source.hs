{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.RestApi.Source
  ( -- * Archive
    ArchiveBlogNode (..),
    ArchiveNode (..),
    ArchivePageEdge (..),
    ArchivePostEdge (..),
    ArchiveBlogEdge (..),
    ArchiveEdge (..),
    ArchiveBlogCollection (..),
    ArchiveCollection (..),
    Archive (..),

    -- * Request
    RequestRecord (..),

    -- ** fetch
    FetchInfo,
    FetchId,

    -- ** http request
    H.RequestId (..),
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
    CollectionInfo,
  )
where

import Data.Vector (Vector)
import Data.Word (Word32)
import Webar.Blob (BlobId)
import Webar.Data.Cbor.TH
import Webar.Data.TH
import qualified Webar.Fetch.Http as F
import qualified Webar.Http as H
import Webar.Object (ObjectId)
import Webar.Server.Wordpress.RestApi.Internal.BlobData (NodeData, SetData)
import Webar.Server.Wordpress.RestApi.Types (ApiInfo)
import Webar.Server.Wordpress.Types
import Webar.Types (Timestamp)

data ArchiveBlogNode
  = AnCategory CategoryId
  | AnComment CommentId
  | AnMedia MediaId
  | AnPage PageId
  | AnPageRevision PageId PageRevisionId
  | AnPost PostId
  | AnPostRevision PostId PostRevisionId
  | AnTag TagId
  deriving (Show, Eq)

data ArchiveNode
  = AnBlog Address ArchiveBlogNode
  | AnUser UserId
  deriving (Show, Eq)

data ArchivePageEdge = APageRevision
  deriving (Show, Eq)

data ArchivePostEdge = APostComment | APostRevision
  deriving (Show, Eq)

data ArchiveBlogEdge
  = AePage PageId {-# UNPACK #-} ArchivePageEdge
  | AePost PostId ArchivePostEdge
  deriving (Show, Eq)

data ArchiveEdge = AeBlog Address ArchiveBlogEdge
  deriving (Show, Eq)

data ArchiveBlogCollection
  = AcCategory
  | AcComment
  | AcMedia
  | AcPage
  | AcPost
  | AcTag
  deriving (Show, Eq)

data ArchiveCollection
  = AcBlog Address ArchiveBlogCollection
  | AcUser
  deriving (Show, Eq)

data Archive
  = ANode ArchiveNode
  | AEdge ArchiveEdge
  | ACollection ArchiveCollection
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( \(n, c) ->
           deriveSumData defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop c} n
       )
       [ (''ArchiveBlogNode, 2),
         (''ArchiveNode, 2),
         (''ArchivePageEdge, 5),
         (''ArchivePostEdge, 5),
         (''ArchiveBlogEdge, 2),
         (''ArchiveEdge, 2),
         (''ArchiveBlogCollection, 2),
         (''ArchiveCollection, 2),
         (''Archive, 1)
       ]
 )

data RequestRecord = RrFetch | RrHttpRequest | RrApiResponse
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''RequestRecord

type FetchInfo = F.FetchInfo ()

type FetchId = F.FetchId ()

type Request = H.Request H.RequestId ()

type Response = H.Response ()

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
      { -- | record node id so failed request can be recorded (e.g. deleted node)
        rdnType :: ArchiveNode,
        rdnResponse :: r
      }
  | RdEdge
      { rdeType :: ArchiveEdge,
        rdeFull :: Bool,
        rdeResponses :: Vector r
      }
  | RdCollection
      { rdcType :: ArchiveCollection,
        rdcFull :: Bool,
        rdcResponses :: Vector r
      }
  deriving (Show)

deriveSumData
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
      constructorTagModifier = camelTo2 '_' . drop 2
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

data SnapshotType = StNode | StEdge {-# UNPACK #-} SnapshotEdge | StCollection
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

type CollectionInfo t = SnapshotInfo (SetContent t)
