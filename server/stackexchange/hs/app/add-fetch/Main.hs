{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Tar as Tar
import Data.Foldable (Foldable (foldl'))
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import Image
import Options.Applicative hiding (info)
import qualified Options.Applicative as OptParse
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Webar.Blob
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json
import Webar.Fetch.Http.Store (Fetch (..), withFetch)
import Webar.Http
import Webar.Object
import Webar.Server.StackExchange.Fetcher.RestClient
import Webar.Server.StackExchange.RestApi.Internal.BlobData
import qualified Webar.Server.StackExchange.RestApi.Model as Api
import Webar.Server.StackExchange.RestApi.Source hiding (Archive, SnapshotType)
import qualified Webar.Server.StackExchange.RestApi.Source as Src
import Webar.Server.StackExchange.RestApi.Types
import Webar.Server.StackExchange.Source
import qualified Webar.Store.Blob.Base as DS.B
import Webar.Store.Blob.WithShared (BlobStore)
import qualified Webar.Store.Blob.WithShared as DS
import qualified Webar.Store.Object.Base as OS.B
import qualified Webar.Store.Object.Website as OS
import Webar.Types (Timestamp, Version (Version))

type ObjectStore = OS.ObjectStore () ArchiveInfo SnapshotType RecordType

data Args = Args
  { argProfileImages :: Maybe FilePath,
    argContentImages :: Maybe FilePath,
    argUnknownUrl :: Maybe FilePath,
    argInvalidUrl :: Maybe FilePath,
    argStoreRoot :: FilePath,
    argServerRoot :: FilePath,
    argFetchRoot :: FilePath
  }

parser :: Parser Args
parser =
  Args
    <$> optional (strOption (long "profile-images" <> metavar "PATH"))
    <*> optional (strOption (long "content-images" <> metavar "PATH"))
    <*> optional (strOption (long "unknown-urls" <> metavar "PATH" <> help "unknown url file"))
    <*> optional (strOption (long "invalid-urls" <> metavar "PATH" <> help "invalid url file"))
    <*> strArgument (metavar "STORE_ROOT" <> help "path to file store")
    <*> strArgument (metavar "SERVER_ROOT" <> help "path to store root")
    <*> strArgument (metavar "FETCH_ROOT" <> help "path to fetch root")

data Context = Context
  { ctxDataStore :: !BlobStore,
    ctxObjectStore :: !ObjectStore,
    ctxFetchId :: !FetchId,
    ctxContentImages :: !Bool,
    ctxProfileImages :: !Bool
  }

data Summary = Summary
  { sumContentImages :: !ImageSet,
    sumProfileImages :: !ImageSet
  }

instance Semigroup Summary where
  l <> r =
    Summary
      { sumContentImages = sumContentImages l <> sumContentImages r,
        sumProfileImages = sumProfileImages l <> sumProfileImages r
      }

instance Monoid Summary where
  mempty =
    Summary
      { sumContentImages = mempty,
        sumProfileImages = mempty
      }

addHttpResponse ::
  Context ->
  Word32 ->
  Int ->
  HttpRequest ->
  IO (ObjectId HttpInfo)
addHttpResponse ctx callSeq respIdx req =
  DS.addBlob ctx.ctxDataStore req.hrResponse.respBody >>= \body ->
    OS.objectId
      <$> OS.addObject
        ctx.ctxObjectStore
        (OtRecord (RtRestApi RrHttpRequest))
        (Version 1 0)
        HttpInfo
          { hiFetch = ctxFetchId ctx,
            hiSeq = callSeq,
            hiResponseIndex = fromIntegral respIdx,
            hiRequest = req.hrRequest,
            hiResponse = (hrResponse req) {respBody = body.blobId}
          }

addApiResponse :: Context -> ApiResponseInfo -> IO (ObjectId ApiResponseInfo)
addApiResponse ctx resp =
  OS.objectId
    <$> OS.addObject ctx.ctxObjectStore (OtRecord (RtRestApi RrApiResponse)) (Version 1 0) resp

addSnapshot :: (Cbor.ToCbor a) => Context -> Src.Archive -> Src.SnapshotType -> a -> IO ()
addSnapshot ctx archive ty meta =
  OS.addObject ctx.ctxObjectStore OtArchive (Version 1 0) (AiRestApi archive) >>= \arch ->
    void
      ( OS.addObject
          ctx.ctxObjectStore
          (OtSnapshot arch.objectId (StRestApi ty))
          (Version 1 0)
          meta
      )

data NodeMeta = NodeMeta
  { nmApiResponseId :: !(ObjectId ApiResponseInfo),
    nmApi :: !ApiInfo,
    nmTimestamp :: !Timestamp
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

addArchiveEdge :: (Cbor.ToCbor a) => Context -> NodeMeta -> Src.ArchiveEdge -> Bool -> S.Set a -> IO ()
addArchiveEdge ctx meta archive full v =
  DS.addBlob ctx.ctxDataStore (SetData (CborData (Cbor.encodeStrictBs v))) >>= \dat ->
    addSnapshot
      ctx
      (Src.AEdge archive)
      (StEdge SeSet)
      SnapshotInfo
        { siFetch = ctx.ctxFetchId,
          siApiResponse = meta.nmApiResponseId,
          siApi = meta.nmApi,
          siTimestamp = meta.nmTimestamp,
          siContent = ScNormal {scContent = dat.blobId, scFull = full}
        }

class (Json.FromJSON a, Cbor.ToCbor (Id a), Ord (Id a)) => Node a where
  type Id a
  archiveInfo :: a -> Src.SiteChildNode
  nodeId :: a -> Id a
  addItemSummary :: Context -> a -> Summary -> Summary

addHtmlContent :: Context -> HtmlContent -> Summary -> Summary
addHtmlContent Context {ctxContentImages = True} (HtmlContent c) s =
  s {sumContentImages = addContentImage c (sumContentImages s)}
addHtmlContent Context {ctxContentImages = False} _ s = s

instance Node Api.Answer where
  type Id Api.Answer = AnswerId
  archiveInfo ans = ScnAnswer ans.ansAnswerId NlNode
  nodeId = Api.ansAnswerId
  addItemSummary ctx ans = addHtmlContent ctx (Api.ansBody ans)

instance Node Api.Badge where
  type Id Api.Badge = BadgeId
  archiveInfo b = ScnBadge b.bBadgeId NlNode
  nodeId = Api.bBadgeId
  addItemSummary ctx b = addHtmlContent ctx (Api.bDescription b)

instance Node Api.Collective where
  type Id Api.Collective = CollectiveSlug
  archiveInfo col = ScnCollective col.colSlug NlNode
  nodeId = Api.colSlug
  addItemSummary
    Context {ctxContentImages = True}
    Api.Collective {Api.colDescription = RawText t}
    s =
      s {sumContentImages = addContentImage t (sumContentImages s)}
  addItemSummary _ _ s = s

instance Node Api.Comment where
  type Id Api.Comment = CommentId
  archiveInfo com = ScnComment com.comCommentId NlNode
  nodeId = Api.comCommentId
  addItemSummary ctx c = addHtmlContent ctx (Api.comBody c)

instance Node Api.Question where
  type Id Api.Question = QuestionId
  archiveInfo q = ScnQuestion q.qQuestionId NlNode
  nodeId = Api.qQuestionId
  addItemSummary ctx q = addHtmlContent ctx (Api.qBody q)

instance Node Api.Tag where
  type Id Api.Tag = TagName
  archiveInfo t = ScnTag t.tagName NlNode
  nodeId = Api.tagName
  addItemSummary _ _ s = s

instance Node Api.TagSynonym where
  type Id Api.TagSynonym = TagName
  archiveInfo t = ScnTagSynonym t.tagSynFromTag NlNode
  nodeId = Api.tagSynFromTag
  addItemSummary _ _ s = s

instance Node Api.TagWiki where
  type Id Api.TagWiki = TagName
  archiveInfo t = ScnTagWiki t.twTagName NlNode
  nodeId = Api.twTagName
  addItemSummary ctx w = addHtmlContent ctx (Api.twBody w)

instance Node Api.User where
  type Id Api.User = UserId
  archiveInfo u = ScnUser u.usrUserId NlNode
  nodeId = Api.usrUserId
  addItemSummary Context {ctxProfileImages = True} Api.User {Api.usrProfileImage = LinkUrl u} s =
    s {sumProfileImages = addImageUrl u (sumProfileImages s)}
  addItemSummary Context {ctxProfileImages = False} _ s = s

instance Node Api.Info where
  type Id Api.Info = ()
  archiveInfo _ = ScnInfo NlNode
  nodeId = const ()
  addItemSummary Context {ctxProfileImages = True} Api.Info {Api.infoSite = obj} s =
    s
      { sumProfileImages =
          let addLink (LinkUrl l) = addImageUrl l
           in ( addLink (Api.sitIconUrl obj)
                  . addLink (Api.sitHighResolutionIconUrl obj)
                  . addLink (Api.sitFaviconUrl obj)
                  . addLink (Api.sitLogoUrl obj)
              )
                s.sumProfileImages
      }
  addItemSummary _ _ s = s

addNode ::
  forall a.
  (Node a) =>
  Context ->
  Summary ->
  ApiSiteParameter ->
  NodeMeta ->
  BinJson.WithBinValue a ->
  IO Summary
addNode ctx s site meta (BinJson.WithBinValue bv obj) =
  addItemSummary ctx obj s
    <$ addArchiveNode ctx meta (AnSite site (NcChild (archiveInfo obj))) bv

data NodeResp = NodeResp
  { nrMeta :: NodeMeta,
    nrNode :: ArchiveNode,
    nrRequest :: HttpRequest
  }

newtype NodeItem t = NodeItem (BinJson.WithBinValue t)

instance (FromJSON t) => FromJSON (NodeItem t) where
  parseJSON =
    withArray
      "NodeItem"
      ( \a ->
          if V.length a == 1
            then NodeItem <$> parseJSON (V.unsafeHead a)
            else fail "expect array with one element"
      )

addNodeResp :: forall a. (Node a) => Context -> Proxy a -> NodeResp -> IO Summary
addNodeResp ctx _ resp@NodeResp {nrNode = AnSite site _} =
  Aeson.throwDecodeStrict resp.nrRequest.hrResponse.respBody.jsonBody
    >>= \Api.Wrapper {Api.wItems = NodeItem bv} ->
      addNode @a ctx mempty site resp.nrMeta bv

data EdgeResp = EdgeResp
  { erMeta :: NodeMeta,
    erEdge :: ArchiveEdge,
    erFull :: Bool,
    erRequest :: Vector HttpRequest
  }

data SummaryWithIds i = SummaryWithIds !Summary !(S.Set i)

addEdgeResp :: forall a. (Node a) => Context -> Proxy a -> EdgeResp -> IO Summary
addEdgeResp ctx _ req = do
  let (ArchiveEdge (EcChild (AceSite site _))) = req.erEdge
  SummaryWithIds s ids <-
    V.foldM'
      ( \si r ->
          Aeson.throwDecodeStrict r.hrResponse.respBody.jsonBody >>= \w ->
            V.foldM'
              ( \(SummaryWithIds s ids) node@(BinJson.WithBinValue _ obj) ->
                  fmap
                    (\updateS -> SummaryWithIds updateS (S.insert (nodeId obj) ids))
                    (addNode @a ctx s site req.erMeta node)
              )
              si
              (Api.wItems w)
      )
      (SummaryWithIds mempty S.empty)
      req.erRequest
  addArchiveEdge ctx req.erMeta req.erEdge req.erFull ids
  pure s

addRevision ::
  Context ->
  Summary ->
  ApiSiteParameter ->
  NodeMeta ->
  RevisionId ->
  [BinJson.WithBinValue Api.Revision] ->
  IO Summary
addRevision ctx summary site meta rId rs =
  ( if ctx.ctxContentImages
      then
        summary
          { sumContentImages =
              foldl'
                ( \imgs (BinJson.WithBinValue _ obj) ->
                    ( addContent obj.revBody
                        . addContent obj.revLastBody
                        . addContent obj.revComment
                    )
                      imgs
                )
                summary.sumContentImages
                rs
          }
      else summary
  )
    <$ addArchiveNode
      ctx
      meta
      (AnSite site (NcChild (ScnRevision rId NlNode)))
      ( fmap
          ( \case
              BinJson.WithBinValue bv _ -> bv
          )
          rs
      )
  where
    addContent (Just (HtmlContent c)) imgs = addContentImage c imgs
    addContent Nothing imgs = imgs

addEntry :: Context -> LBS.ByteString -> IO Summary
addEntry ctx bs =
  let entry = Cbor.decodeLazyBsThrow bs
   in case arData entry of
        RdNode {rdnType = ty@(AnSite _ (NcChild child)), rdnResponse = resp} -> do
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
          let nodeResp =
                NodeResp
                  { nrMeta =
                      NodeMeta
                        { nmApiResponseId = rId,
                          nmApi = entry.arApi,
                          nmTimestamp = timestamp
                        },
                    nrNode = ty,
                    nrRequest = resp
                  }
          case child of
            ScnAnswer _ _ -> addNodeResp @Api.Answer ctx Proxy nodeResp
            ScnBadge _ _ -> addNodeResp @Api.Badge ctx Proxy nodeResp
            ScnCollective _ _ -> addNodeResp @Api.Collective ctx Proxy nodeResp
            ScnComment _ _ -> addNodeResp @Api.Comment ctx Proxy nodeResp
            ScnInfo _ -> addNodeResp @Api.Info ctx Proxy nodeResp
            ScnQuestion _ _ -> addNodeResp @Api.Question ctx Proxy nodeResp
            ScnRevision _ _ -> error "revision shouldn't be fetched as node"
            ScnTag _ _ -> addNodeResp @Api.Tag ctx Proxy nodeResp
            ScnTagSynonym _ _ -> addNodeResp @Api.TagSynonym ctx Proxy nodeResp
            ScnTagWiki _ _ -> addNodeResp @Api.TagWiki ctx Proxy nodeResp
            ScnUser _ _ -> addNodeResp @Api.User ctx Proxy nodeResp
        RdEdge
          { rdeType = ty@(ArchiveEdge (EcChild (AceSite _ child))),
            rdeFull = full,
            rdeResponses = resps
          } -> do
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
            let edgeResp =
                  EdgeResp
                    { erMeta =
                        NodeMeta
                          { nmApiResponseId = rId,
                            nmApi = entry.arApi,
                            nmTimestamp = timestamp
                          },
                      erEdge = ty,
                      erFull = full,
                      erRequest = resps
                    }
            case child of
              EbChild c -> case c of
                SceAnswer _ (ElEdge a) -> case a of
                  AnsComment -> addEdgeResp @Api.Comment ctx Proxy edgeResp
                  AnsRevision -> error "revision list shouldn't be fetched as edge"
                SceCollective _ (ElEdge col) -> case col of
                  ColAnswer -> addEdgeResp @Api.Answer ctx Proxy edgeResp
                  ColQuestion -> addEdgeResp @Api.Question ctx Proxy edgeResp
                  ColTag -> addEdgeResp @Api.Tag ctx Proxy edgeResp
                  ColUser -> addEdgeResp @Api.User ctx Proxy edgeResp
                SceQuestion _ (ElEdge q) -> case q of
                  QueAnswer -> addEdgeResp @Api.Answer ctx Proxy edgeResp
                  QueComment -> addEdgeResp @Api.Comment ctx Proxy edgeResp
                  QueRevision -> error "revision list shouldn't be fetched as edge"
                SceTag _ (ElEdge t) -> case t of
                  TTagSynonym -> addEdgeResp @Api.TagSynonym ctx Proxy edgeResp
                SceUser _ (ElEdge u) -> case u of
                  UsrAnswer -> addEdgeResp @Api.Answer ctx Proxy edgeResp
                  UsrBadge -> addEdgeResp @Api.Badge ctx Proxy edgeResp
                  UsrComment -> addEdgeResp @Api.Comment ctx Proxy edgeResp
                  UsrQuestion -> addEdgeResp @Api.Question ctx Proxy edgeResp
              EbEdge e -> case e of
                SitBadge -> addEdgeResp @Api.Badge ctx Proxy edgeResp
                SitTag -> addEdgeResp @Api.Tag ctx Proxy edgeResp
        RdRevision {rdrType = ty, rdrResponses = resps} -> do
          let timestamp = (V.head resps).hrRequest.reqTimestamp
          rId <-
            V.imapM (addHttpResponse ctx entry.arSeq) resps >>= \hIds ->
              addApiResponse
                ctx
                ApiResponseInfo
                  { Src.arFetch = ctx.ctxFetchId,
                    Src.arSeq = entry.arSeq,
                    Src.arApi = entry.arApi,
                    Src.arTimestamp = timestamp,
                    Src.arResponse = RdRevision ty hIds
                  }
          items <-
            V.mapM
              ( \r ->
                  Api.wItems @(V.Vector (BinJson.WithBinValue Api.Revision))
                    <$> Aeson.throwDecodeStrict r.hrResponse.respBody.jsonBody
              )
              resps
          let nodeMeta =
                NodeMeta
                  { nmApiResponseId = rId,
                    nmApi = entry.arApi,
                    nmTimestamp = timestamp
                  }
          case ty of
            ANode (AnSite site (NcChild (ScnRevision revId _))) ->
              addRevision ctx mempty site nodeMeta revId (concatMap V.toList items)
            ANode _ -> error "invalid revision node type"
            AEdge edgeTy@(ArchiveEdge (EcChild (AceSite site _))) -> do
              let revMap =
                    V.foldr'
                      ( \revs mp0 ->
                          V.foldr'
                            ( \bv@(BinJson.WithBinValue _ rev) ->
                                M.alter
                                  ( \case
                                      Just bvs -> Just (bv : bvs)
                                      Nothing -> Just [bv]
                                  )
                                  rev.revRevisionGuid
                            )
                            mp0
                            revs
                      )
                      M.empty
                      items
              summary <-
                M.foldlWithKey'
                  ( \m revId rev ->
                      m >>= \s -> addRevision ctx s site nodeMeta revId rev
                  )
                  (pure mempty)
                  revMap
              addArchiveEdge ctx nodeMeta edgeTy True (M.keysSet revMap)
              pure summary

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

withObjectStore :: FilePath -> FilePath -> (ObjectStore -> IO c) -> IO c
withObjectStore root serverRoot f =
  bracket
    (OS.B.openByFilePath (root </> "store/object"))
    OS.B.close
    ( \baseObj ->
        bracket
          (OS.openByFilePath server () baseObj (serverRoot </> "store/object"))
          OS.close
          f
    )

main :: IO ()
main = do
  args <- execParser (OptParse.info (parser <**> helper) mempty)
  summary <-
    withDataStore
      args.argStoreRoot
      args.argServerRoot
      ( \ds ->
          withFetch @() @FetchType @()
            ds
            server
            FtRestApi
            args.argFetchRoot
            ( \f ->
                withObjectStore
                  args.argStoreRoot
                  args.argServerRoot
                  ( \os -> do
                      fetchId <- OS.addObject os (OtRecord (RtRestApi RrFetch)) (Version 1 0) f.fInfo
                      let context =
                            Context
                              { ctxDataStore = ds,
                                ctxObjectStore = os,
                                ctxFetchId = fetchId.objectId,
                                ctxContentImages = isJust (argContentImages args),
                                ctxProfileImages = isJust (argProfileImages args)
                              }
                      C.runConduit
                        ( C.sourceHandle f.fData
                            .| Tar.untar
                              ( \fi ->
                                  C.map BSB.byteString .| C.sinkLazyBuilder >>= \ent ->
                                    liftIO
                                      ( catch
                                          ( do
                                              s <- addEntry context ent
                                              let ctx = show (Tar.filePath fi)
                                              printError ctx (sumContentImages s)
                                              printError ctx (sumProfileImages s)
                                              print (Tar.filePath fi)
                                              pure s
                                          )
                                          ( \(SomeException exn) ->
                                              mempty
                                                <$ hPutStrLn
                                                  stderr
                                                  (show (Tar.filePath fi) ++ ": " ++ displayException exn)
                                          )
                                      )
                                      >>= C.yield
                              )
                            .| C.fold
                        )
                  )
            )
      )
  writeImageSpec (argContentImages args) (sumContentImages summary)
  writeImageSpec (argProfileImages args) (sumProfileImages summary)
  writeUrlSet
    (argInvalidUrl args)
    ( M.keysSet
        ( invalidUrls (sumContentImages summary)
            <> invalidUrls (sumProfileImages summary)
        )
    )
  writeUrlSet
    (argUnknownUrl args)
    ( unknownUrls (sumContentImages summary)
        <> unknownUrls (sumProfileImages summary)
    )
