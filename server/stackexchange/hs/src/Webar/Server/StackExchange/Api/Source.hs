{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Source where

import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Word (Word32)
import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Cbor.TH (deriveProdCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import Webar.Digest
import Webar.Fetch.Http (FetchId)
import qualified Webar.Http as H
import Webar.Server.StackExchange.Types (AnswerId, ApiSiteParameter, CollectiveSlug, FilterId, QuestionId, RevisionId, TagName, UserId)
import Webar.Types

data ApiVersion
  = Api2_3
  deriving (Show, Eq, Ord)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = fmap (\c -> if c == '_' then '.' else c) . drop 3
    }
  ''ApiVersion

newtype RequestId = ReqXRequestId UUID
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '-' . drop 3
    }
  ''RequestId

newtype ResponseId = RespXRequestGuid UUID
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '-' . drop 4
    }
  ''ResponseId

type Response = H.Response (Maybe ResponseId)

data HttpInfo = HttpInfo
  { hiUrl :: Text,
    hiFetch :: FetchId,
    hiCallSeq :: Word32,
    hiResponseIndex :: Word32,
    hiRequestId :: RequestId,
    hiResponse :: Response Digest
  }
  deriving (Show)

deriveProdCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpInfo

newtype HttpResponseId = HttpResponseId Digest
  deriving (Show, Eq, Ord, FromJSON, ToJSON, FromCbor, ToCbor)

data ObjectType
  = OtAnswer
  | OtBadge
  | OtCollective
  | OtComment
  | OtFilter
  | OtQuestion
  | OtRevision
  | OtTag
  | OtTagSynonym
  | OtTagWiki
  | OtUser
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ObjectType

data AnswerListReq = AlComment | AlRevision
  deriving (Show, Eq)

data CollectiveListReq = ClAnswer | ClQuestion | ClTag | ClUser
  deriving (Show, Eq)

data QuestionListReq = QlAnswer | QlComment | QlRevision
  deriving (Show, Eq)

data TagListReq = TlTagSynonym
  deriving (Show, Eq)

data UserListReq = UlAnswer | UlBadge | UlComment | UlQuestion
  deriving (Show, Eq)

$( concat
     <$> traverse
       ( deriveSumData
           SumOptions
             { sumProduct = ProductOptions {fieldLabelModifier = id},
               constructorTagModifier = camelTo2 '_' . drop 2
             }
       )
       [ ''AnswerListReq,
         ''CollectiveListReq,
         ''QuestionListReq,
         ''TagListReq,
         ''UserListReq
       ]
 )

data ListRequest
  = LrAnswer
      { lraId :: AnswerId,
        lraRequest :: AnswerListReq
      }
  | LrCollective
      { lrcId :: CollectiveSlug,
        lrcRequest :: CollectiveListReq
      }
  | LrListRevision RevisionId
  | LrQuestion
      { lrqId :: QuestionId,
        lrqRequest :: QuestionListReq
      }
  | LrTag
      { lrtId :: TagName,
        lrtRequest :: TagListReq
      }
  | LrUser
      { lruId :: UserId,
        lruRequest :: UserListReq
      }
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct =
        ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ListRequest

data ResponseData r
  = RdObjects
      { rdoType :: ObjectType,
        rdoResponse :: r
      }
  | RdList
      { rdlRequest :: ListRequest,
        rdlFull :: Bool,
        rdlResponses :: Vector r
      }
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''ResponseData

data ApiInfo = ApiInfo
  { apiCallSeq :: Word32,
    apiVersion :: ApiVersion,
    apiSite :: ApiSiteParameter,
    apiTimestamp :: Timestamp,
    apiFilter :: FilterId,
    apiResponse :: ResponseData HttpResponseId
  }
  deriving (Show)

newtype ApiResponseId = ApiResponseId Digest
  deriving (Show, Eq, Ord, FromJSON, ToJSON, FromCbor, ToCbor)

data SnapshotId = SnapshotId
  { siApiResponse :: ApiResponseId,
    siIndex :: Maybe Word
  }
  deriving (Show, Eq, Ord)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''SnapshotId

newtype Content
  = CNormal Digest
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Content

data Metadata = Metadata
  { metaId :: SnapshotId,
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
  { lcContent :: Digest,
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
  { listId :: ApiResponseId,
    listContent :: ListContent,
    listApiVersion :: ApiVersion,
    listTimestamp :: Timestamp
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''ListMeta