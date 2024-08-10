{-# LANGUAGE TemplateHaskell #-}

-- | generic image store objects
module Webar.Image.Source
  ( F.FetchInfo (..),
    FetchId,
    RequestId (..),
    Request,
    Response,
    HttpInfo (..),
    RequestRecord (..),
    Content (..),
    Image (..),
    SnapshotType (..),
  )
where

import Data.UUID.Types (UUID)
import Data.Void (Void)
import Webar.Blob
import Webar.Data.Cbor.TH (deriveProdCbor)
import Webar.Data.TH
import qualified Webar.Fetch.Http as F
import qualified Webar.Http as H
import Webar.Object (ObjectId)
import Webar.Types (Timestamp)

type FetchId = F.FetchId Void

newtype RequestId = XRequestId UUID
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '-'
    }
  ''RequestId

type Request = H.Request RequestId ()

type Response = H.Response () (BlobId ImageData)

data HttpInfo = HttpInfo
  { hiFetch :: FetchId,
    hiRequest :: Request,
    hiResponse :: Response
  }
  deriving (Show)

deriveProdCbor
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''HttpInfo

data RequestRecord = RrFetch | RrHttpRequest
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''RequestRecord

newtype Content = CNormal (BlobId ImageData)
  deriving (Show)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . tail
    }
  ''Content

data Image = Image
  { imgFetch :: FetchId,
    imgRequest :: ObjectId HttpInfo,
    imgTimestamp :: Timestamp,
    imgContent :: Content
  }
  deriving (Show)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Image

data SnapshotType = IsImage
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SnapshotType