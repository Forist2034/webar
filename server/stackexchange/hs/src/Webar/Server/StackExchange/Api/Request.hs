{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Request where

import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word32)
import Webar.Data.Cbor.TH
import Webar.Data.TH
import qualified Webar.Fetch.Http as F
import qualified Webar.Http as H
import Webar.Object
import Webar.Server.StackExchange.Api.Filter (FilterId)
import Webar.Server.StackExchange.Api.Types
import Webar.Types (Timestamp)

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

type FetchId = F.FetchId Void

data HttpInfo = HttpInfo
  { hiUrl :: Text,
    hiFetch :: FetchId,
    hiCallSeq :: Word32,
    hiResponseIndex :: Word32,
    hiRequestId :: RequestId,
    hiResponse :: Response DataId
  }
  deriving (Show)

deriveProdCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpInfo

type HttpResponseId = ObjectId HttpInfo

data ApiObjectType
  = OtAnswer
  | OtBadge
  | OtCollective
  | OtComment
  | OtInfo
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
  ''ApiObjectType

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
      { rdoType :: ApiObjectType,
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
  { apiFetch :: FetchId,
    apiCallSeq :: Word32,
    apiVersion :: ApiVersion,
    apiSite :: ApiSiteParameter,
    apiTimestamp :: Timestamp,
    apiFilter :: FilterId,
    apiResponse :: ResponseData HttpResponseId
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ApiInfo

type ApiResponseId = ObjectId ApiInfo