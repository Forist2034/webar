{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.RestApi.Source
  ( -- * Archive
    Archive (..),

    -- ** node
    NodeLeaf (..),
    NodeChild (..),
    BlogChildNode (..),
    ArchiveNode (..),

    -- ** edge
    EdgeLeaf (..),
    EdgeChild (..),
    EdgeBranch (..),
    PostEdge (..),
    BlogChildEdge (..),
    BlogEdge (..),
    ArchiveChildEdge (..),
    ArchiveSelfEdge (..),
    ArchiveEdge (..),

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
  )
where

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
import Webar.Server.Wordpress.RestApi.Internal.BlobData (NodeData, SetData)
import Webar.Server.Wordpress.RestApi.Types (ApiInfo)
import Webar.Server.Wordpress.Types
import Webar.Types (Timestamp)

data NodeLeaf = NlNode
  deriving (Show, Eq)

newtype NodeChild c = NcChild c
  deriving (Show, Eq)

data BlogChildNode
  = BcnCategory CategoryId {-# UNPACK #-} NodeLeaf
  | BcnComment CommentId {-# UNPACK #-} NodeLeaf
  | BcnMedia MediaId {-# UNPACK #-} NodeLeaf
  | BcnPage PageId {-# UNPACK #-} NodeLeaf
  | BcnPost PostId {-# UNPACK #-} NodeLeaf
  | BcnTag TagId {-# UNPACK #-} NodeLeaf
  deriving (Show, Eq)

data ArchiveNode
  = AnBlog Address (NodeChild BlogChildNode)
  | AnUser UserId {-# UNPACK #-} NodeLeaf
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
         (''BlogChildNode, 3),
         (''ArchiveNode, 2)
       ]
 )

newtype EdgeLeaf e = ElEdge e
  deriving (Show, Eq)

newtype EdgeChild c = EcChild c
  deriving (Show, Eq)

data EdgeBranch e c = EbEdge e | EbChild c
  deriving (Show, Eq)

data PostEdge = APostComment
  deriving (Show, Eq)

data BlogChildEdge
  = BcePost PostId (EdgeLeaf PostEdge)
  deriving (Show, Eq)

data BlogEdge
  = BeCategory
  | BeComment
  | BeMedia
  | BePage
  | BePost
  | BeTag
  deriving (Show, Eq)

data ArchiveChildEdge
  = AceBlog Address (EdgeBranch BlogEdge BlogChildEdge)
  deriving (Show, Eq)

data ArchiveSelfEdge = AseUser
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( \(n, c) ->
           deriveSumData defaultSumOptions {constructorTagModifier = camelTo2 '_' . drop c} n
       )
       [ (''EdgeLeaf, 2),
         (''EdgeChild, 2),
         (''EdgeBranch, 2),
         (''PostEdge, 5),
         (''BlogChildEdge, 3),
         (''BlogEdge, 2),
         (''ArchiveChildEdge, 3),
         (''ArchiveSelfEdge, 3)
       ]
 )

newtype ArchiveEdge = ArchiveEdge (EdgeBranch ArchiveSelfEdge ArchiveChildEdge)
  deriving (Show, Eq, FromCbor, ToCbor, FromJSON, ToJSON)

data Archive
  = ANode ArchiveNode
  | AEdge ArchiveEdge
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions {constructorTagModifier = camelTo2 '_' . tail}
  ''Archive

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
