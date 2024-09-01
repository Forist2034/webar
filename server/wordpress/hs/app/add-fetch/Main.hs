{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Tar as Tar
import Data.Functor
import Data.Proxy
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import Options.Applicative
import qualified Options.Applicative as OptParse
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Webar.Blob (BinJsonData (..), CborData (CborData))
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json
import Webar.Fetch.Http.Store
import Webar.Http
import Webar.Object
import Webar.Server.Wordpress.Fetcher.RestClient
import Webar.Server.Wordpress.RestApi.Internal.BlobData
import qualified Webar.Server.Wordpress.RestApi.Model as Api
import Webar.Server.Wordpress.RestApi.Source hiding
  ( Archive (..),
    SnapshotType (..),
  )
import qualified Webar.Server.Wordpress.RestApi.Source as Src
import Webar.Server.Wordpress.RestApi.Types (ApiInfo)
import Webar.Server.Wordpress.Source
import Webar.Server.Wordpress.Types
import qualified Webar.Store.Blob.Base as DS.B
import Webar.Store.Blob.WithShared (BlobStore)
import qualified Webar.Store.Blob.WithShared as DS
import qualified Webar.Store.Object.Base as OS.B
import qualified Webar.Store.Object.Website as OS
import Webar.Types (Timestamp, Version (Version))

data Args = Args
  { argStoreRoot :: FilePath,
    argServerRoot :: FilePath,
    argFetchRoot :: FilePath
  }

parser :: Parser Args
parser =
  Args
    <$> strArgument (metavar "STORE_ROOT")
    <*> strArgument (metavar "SERVER_ROOT")
    <*> strArgument (metavar "FETCH_ROOT")

type ObjectStore = OS.ObjectStore Instance Archive SnapshotType RecordType

data Context = Context
  { ctxFetchId :: Src.FetchId,
    ctxDataStore :: BlobStore,
    ctxObjectStore :: ObjectStore
  }

addHttpResponse :: Context -> Word32 -> Int -> HttpRequest -> IO (ObjectId HttpInfo)
addHttpResponse ctx callSeq idx resp = do
  body <- DS.blobId <$> DS.addBlob ctx.ctxDataStore resp.hrResponse.respBody
  OS.objectId
    <$> OS.addObject
      ctx.ctxObjectStore
      (OtRecord (RtRestApi Src.RrHttpRequest))
      (Version 1 0)
      HttpInfo
        { hiFetch = ctx.ctxFetchId,
          hiSeq = callSeq,
          hiResponseIndex = fromIntegral idx,
          hiRequest = resp.hrRequest,
          hiResponse = resp.hrResponse {respBody = body}
        }

addApiResponse :: Context -> ApiResponseInfo -> IO (ObjectId ApiResponseInfo)
addApiResponse ctx resp =
  OS.objectId
    <$> OS.addObject
      ctx.ctxObjectStore
      (OtRecord (RtRestApi Src.RrApiResponse))
      (Version 1 0)
      resp

addSnapshot :: (Cbor.ToCbor a) => Context -> Src.Archive -> Src.SnapshotType -> a -> IO ()
addSnapshot ctx archive ty meta =
  OS.addObject ctx.ctxObjectStore OtArchive (Version 1 0) (ARestApi archive) >>= \arch ->
    void
      ( OS.addObject
          ctx.ctxObjectStore
          (OtSnapshot arch.objectId (StRestApi ty))
          (Version 1 0)
          meta
      )

data NodeMeta = NodeMeta
  { nmApiResponseId :: ObjectId ApiResponseInfo,
    nmApi :: ApiInfo,
    nmTimestamp :: Timestamp
  }

addArchiveNode :: (Cbor.ToCbor a) => Context -> NodeMeta -> Src.ArchiveNode -> a -> IO ()
addArchiveNode ctx meta archive bv =
  DS.addBlob ctx.ctxDataStore (NodeData (BinJsonData (Cbor.encodeStrictBs bv))) >>= \dat ->
    addSnapshot
      ctx
      (Src.ANode archive)
      Src.StNode
      SnapshotInfo
        { siFetch = ctx.ctxFetchId,
          siApiResponse = meta.nmApiResponseId,
          siApi = meta.nmApi,
          siTimestamp = meta.nmTimestamp,
          siContent = NcNormal dat.blobId
        }

addArchiveSet :: (Cbor.ToCbor a) => Context -> NodeMeta -> Src.Archive -> Src.SnapshotType -> Bool -> S.Set a -> IO ()
addArchiveSet ctx meta archive ty full v =
  DS.addBlob ctx.ctxDataStore (SetData (CborData (Cbor.encodeStrictBs v))) >>= \dat ->
    addSnapshot
      ctx
      archive
      ty
      SnapshotInfo
        { siFetch = ctx.ctxFetchId,
          siApiResponse = meta.nmApiResponseId,
          siApi = meta.nmApi,
          siTimestamp = meta.nmTimestamp,
          siContent = ScNormal {scContent = dat.blobId, scFull = full}
        }

class (Json.FromJSON n, Cbor.ToCbor (Id n), Ord (Id n)) => Node n where
  type Addr n
  type Id n
  archiveNode :: Addr n -> n -> Src.ArchiveNode
  nodeId :: n -> Id n

instance Node Api.Category where
  type Addr Api.Category = Address
  type Id Api.Category = CategoryId
  archiveNode a c = Src.AnBlog a (Src.AnCategory c.catId)
  nodeId c = c.catId

instance Node Api.Comment where
  type Addr Api.Comment = Address
  type Id Api.Comment = CommentId
  archiveNode a c = Src.AnBlog a (Src.AnComment c.comId)
  nodeId c = c.comId

instance Node Api.Media where
  type Addr Api.Media = Address
  type Id Api.Media = MediaId
  archiveNode a m = Src.AnBlog a (Src.AnMedia m.medId)
  nodeId m = m.medId

instance Node Api.Page where
  type Addr Api.Page = Address
  type Id Api.Page = PageId
  archiveNode a p = Src.AnBlog a (Src.AnPage p.pageId)
  nodeId p = p.pageId

instance Node Api.PageRevision where
  type Addr Api.PageRevision = Address
  type Id Api.PageRevision = PageRevisionId
  archiveNode a p = Src.AnBlog a (Src.AnPageRevision p.pagRevParent p.pagRevId)
  nodeId p = p.pagRevId

instance Node Api.Post where
  type Addr Api.Post = Address
  type Id Api.Post = PostId
  archiveNode a p = Src.AnBlog a (Src.AnPost p.postId)
  nodeId p = p.postId

instance Node Api.PostRevision where
  type Addr Api.PostRevision = Address
  type Id Api.PostRevision = PostRevisionId
  archiveNode a p = Src.AnBlog a (Src.AnPostRevision p.pRevParent p.pRevId)
  nodeId p = p.pRevId

instance Node Api.Tag where
  type Addr Api.Tag = Address
  type Id Api.Tag = TagId
  archiveNode a t = Src.AnBlog a (Src.AnTag t.tagId)
  nodeId t = t.tagId

instance Node Api.User where
  type Addr Api.User = ()
  type Id Api.User = UserId
  archiveNode _ u = Src.AnUser u.usrId
  nodeId u = u.usrId

addNode :: (Node a) => Context -> NodeMeta -> Addr a -> BinJson.WithBinValue a -> IO ()
addNode ctx meta addr (BinJson.WithBinValue bv obj) =
  addArchiveNode ctx meta (archiveNode addr obj) bv

data NodeResp a = NodeResp
  { nrMeta :: NodeMeta,
    nrAddr :: a,
    nrRequest :: HttpRequest
  }

addNodeResp :: forall a. (Node a) => Context -> Proxy a -> NodeResp (Addr a) -> IO ()
addNodeResp ctx _ resp =
  Aeson.throwDecodeStrict resp.nrRequest.hrResponse.respBody.jsonBody
    >>= addNode @a ctx resp.nrMeta resp.nrAddr

data SetResp = SetResp
  { srMeta :: NodeMeta,
    srFull :: Bool,
    srRequest :: Vector HttpRequest
  }

addSetResp :: forall a. (Node a) => Context -> Src.Archive -> Src.SnapshotType -> Proxy a -> Addr a -> SetResp -> IO ()
addSetResp ctx archive st _ addr req = do
  ids <-
    V.foldM'
      ( \si r ->
          Aeson.throwDecodeStrict r.hrResponse.respBody.jsonBody >>= \nodes ->
            V.foldM'
              ( \ids node@(BinJson.WithBinValue _ obj) ->
                  S.insert (nodeId obj) ids
                    <$ addNode @a ctx req.srMeta addr node
              )
              si
              nodes
      )
      S.empty
      req.srRequest
  addArchiveSet ctx req.srMeta archive st req.srFull ids

addEdgeResp :: (Node a) => Context -> ArchiveEdge -> Proxy a -> Addr a -> SetResp -> IO ()
addEdgeResp ctx archive = addSetResp ctx (Src.AEdge archive) (Src.StEdge SeSet)

addCollectionResp :: (Node a) => Context -> ArchiveCollection -> Proxy a -> Addr a -> SetResp -> IO ()
addCollectionResp ctx archive = addSetResp ctx (Src.ACollection archive) Src.StCollection

addEntry :: Context -> LBS.ByteString -> IO ()
addEntry ctx bs =
  let entry = Cbor.decodeLazyBsThrow bs
   in case arData entry of
        Src.RdNode {Src.rdnType = ty, Src.rdnResponse = resp} -> do
          let timestamp = resp.hrRequest.reqTimestamp
          rId <-
            addHttpResponse ctx entry.arSeq 0 resp >>= \hId ->
              addApiResponse
                ctx
                Src.ApiResponseInfo
                  { Src.arFetch = ctx.ctxFetchId,
                    Src.arTimestamp = timestamp,
                    Src.arSeq = entry.arSeq,
                    Src.arApi = entry.arApi,
                    Src.arResponse =
                      Src.RdNode {Src.rdnType = ty, Src.rdnResponse = hId}
                  }
          let meta =
                NodeMeta
                  { nmApiResponseId = rId,
                    nmApi = entry.arApi,
                    nmTimestamp = timestamp
                  }
          case ty of
            NtBlog addr bt ->
              let nodeResp = NodeResp {nrMeta = meta, nrAddr = addr, nrRequest = resp}
               in case bt of
                    NtCategory -> addNodeResp @Api.Category ctx Proxy nodeResp
                    NtComment -> addNodeResp @Api.Comment ctx Proxy nodeResp
                    NtMedia -> addNodeResp @Api.Media ctx Proxy nodeResp
                    NtPage -> addNodeResp @Api.Page ctx Proxy nodeResp
                    NtPageRevision -> addNodeResp @Api.PageRevision ctx Proxy nodeResp
                    NtPost -> addNodeResp @Api.Post ctx Proxy nodeResp
                    NtPostRevision -> addNodeResp @Api.PostRevision ctx Proxy nodeResp
                    NtTag -> addNodeResp @Api.Tag ctx Proxy nodeResp
            NtUser ->
              addNodeResp @Api.User
                ctx
                Proxy
                NodeResp {nrMeta = meta, nrAddr = (), nrRequest = resp}
        Src.RdEdge {Src.rdeType = ty, Src.rdeFull = full, Src.rdeResponses = resps} -> do
          let timestamp = (V.head resps).hrRequest.reqTimestamp
          rId <-
            V.imapM (addHttpResponse ctx entry.arSeq) resps >>= \hIds ->
              addApiResponse
                ctx
                Src.ApiResponseInfo
                  { Src.arFetch = ctx.ctxFetchId,
                    Src.arTimestamp = timestamp,
                    Src.arSeq = entry.arSeq,
                    Src.arApi = entry.arApi,
                    Src.arResponse =
                      Src.RdEdge
                        { Src.rdeType = ty,
                          Src.rdeFull = full,
                          Src.rdeResponses = hIds
                        }
                  }
          let setResp =
                SetResp
                  { srMeta = NodeMeta {nmApiResponseId = rId, nmApi = entry.arApi, nmTimestamp = timestamp},
                    srFull = full,
                    srRequest = resps
                  }
          case ty of
            AeBlog addr et -> case et of
              AePage _ APageRevision -> addEdgeResp @Api.PageRevision ctx ty Proxy addr setResp
              AePost _ pe -> case pe of
                APostComment -> addEdgeResp @Api.Comment ctx ty Proxy addr setResp
                APostRevision -> addEdgeResp @Api.PostRevision ctx ty Proxy addr setResp
        Src.RdCollection {Src.rdcType = ty, Src.rdcFull = full, Src.rdcResponses = resps} -> do
          let timestamp = (V.head resps).hrRequest.reqTimestamp
          rId <-
            V.imapM (addHttpResponse ctx entry.arSeq) resps >>= \hIds ->
              addApiResponse
                ctx
                Src.ApiResponseInfo
                  { Src.arFetch = ctx.ctxFetchId,
                    Src.arTimestamp = timestamp,
                    Src.arSeq = entry.arSeq,
                    Src.arApi = entry.arApi,
                    Src.arResponse =
                      Src.RdCollection
                        { Src.rdcType = ty,
                          Src.rdcFull = full,
                          Src.rdcResponses = hIds
                        }
                  }
          let setResp =
                SetResp
                  { srMeta = NodeMeta {nmApiResponseId = rId, nmApi = entry.arApi, nmTimestamp = timestamp},
                    srFull = full,
                    srRequest = resps
                  }
          case ty of
            AcBlog addr bc -> case bc of
              AcCategory -> addCollectionResp @Api.Category ctx ty Proxy addr setResp
              AcComment -> addCollectionResp @Api.Comment ctx ty Proxy addr setResp
              AcMedia -> addCollectionResp @Api.Media ctx ty Proxy addr setResp
              AcPage -> addCollectionResp @Api.Media ctx ty Proxy addr setResp
              AcPost -> addCollectionResp @Api.Post ctx ty Proxy addr setResp
              AcTag -> addCollectionResp @Api.Tag ctx ty Proxy addr setResp
            AcUser -> addCollectionResp @Api.User ctx ty Proxy () setResp

withDataStore :: FilePath -> FilePath -> (BlobStore -> IO c) -> IO c
withDataStore root serverRoot f =
  bracket
    (DS.B.openByFilePath (root </> "store/blob"))
    DS.B.close
    ( \baseData ->
        bracket
          (DS.openByFilePath baseData (serverRoot </> "store/blob"))
          DS.close
          f
    )

withObjectStore :: FilePath -> FilePath -> Instance -> (ObjectStore -> IO c) -> IO c
withObjectStore root serverRoot inst f =
  bracket
    (OS.B.openByFilePath (root </> "store/object"))
    OS.B.close
    ( \baseObj ->
        bracket
          (OS.openByFilePath server inst baseObj (serverRoot </> "store/object"))
          OS.close
          f
    )

main :: IO ()
main = do
  args <- execParser (OptParse.info (parser <**> helper) mempty)
  withDataStore args.argStoreRoot args.argServerRoot \ds ->
    withFetch @Instance @FetchType @() ds server FtRestApi args.argFetchRoot \f ->
      withObjectStore args.argStoreRoot args.argServerRoot f.fInstance \os -> do
        fetchId <- OS.addObject os (OtRecord (RtRestApi Src.RrFetch)) (Version 1 0) f.fInfo
        let context =
              Context
                { ctxFetchId = fetchId.objectId,
                  ctxDataStore = ds,
                  ctxObjectStore = os
                }
        C.runConduit
          ( C.sourceHandle f.fData .| Tar.untar \fi ->
              C.map BSB.byteString .| C.sinkLazyBuilder >>= \ent ->
                liftIO
                  ( catch
                      ( do
                          addEntry context ent
                          print (Tar.filePath fi)
                      )
                      ( \(SomeException exn) ->
                          hPutStrLn
                            stderr
                            (show (Tar.filePath fi) ++ ": " ++ displayException exn)
                      )
                  )
          )
