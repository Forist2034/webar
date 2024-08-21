{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Tar as Tar
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
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
import Webar.Server.StackExchange.Api.Filter (FilterId)
import Webar.Server.StackExchange.Api.Internal.BlobData
import qualified Webar.Server.StackExchange.Api.Model as Api
import Webar.Server.StackExchange.Api.Request
import Webar.Server.StackExchange.Api.Source
import Webar.Server.StackExchange.Api.Types
import Webar.Server.StackExchange.Fetcher.ApiClient
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
  IO HttpResponseId
addHttpResponse ctx callSeq respIdx req =
  DS.addBlob ctx.ctxDataStore req.hrResponse.respBody >>= \body ->
    OS.objectId
      <$> OS.addObject
        ctx.ctxObjectStore
        (OtRecord (RtApiRequest RrHttpRequest))
        (Version 1 0)
        HttpInfo
          { hiUrl = req.hrRequest.reqUrl,
            hiFetch = ctxFetchId ctx,
            hiRequestId = req.hrRequest.reqId,
            hiCallSeq = callSeq,
            hiResponseIndex = fromIntegral respIdx,
            hiResponse = (hrResponse req) {respBody = body.blobId}
          }

addApiResponse :: Context -> ApiInfo -> IO ApiResponseId
addApiResponse ctx resp =
  OS.objectId
    <$> OS.addObject ctx.ctxObjectStore (OtRecord (RtApiRequest RrApiResponse)) (Version 1 0) resp

addSnapshot :: (Cbor.ToCbor a) => Context -> ArchiveInfo -> SnapshotType -> a -> IO ()
addSnapshot ctx archive ty meta =
  OS.addObject ctx.ctxObjectStore OtArchive (Version 1 0) archive >>= \arch ->
    void (OS.addObject ctx.ctxObjectStore (OtSnapshot arch.objectId ty) (Version 1 0) meta)

data ItemMeta = ItemMeta
  { imSite :: !ApiSiteParameter,
    imApiResponseId :: !ApiResponseId,
    imApiVersion :: !ApiVersion,
    imFilter :: !FilterId,
    imTimestamp :: !Timestamp
  }

addApiItem :: (Cbor.ToCbor a) => Context -> ItemMeta -> ArchiveSiteData -> a -> IO ()
addApiItem ctx meta archive bv =
  DS.addBlob ctx.ctxDataStore (ApiData (BinJsonData (Cbor.encodeStrictBs bv))) >>= \dat ->
    addSnapshot
      ctx
      (AiSiteApi meta.imSite archive)
      (StApi AstObject)
      ObjectMeta
        { objFetch = ctx.ctxFetchId,
          objApiResponse = meta.imApiResponseId,
          objContent = CNormal dat.blobId,
          objApiVersion = meta.imApiVersion,
          objFilter = meta.imFilter,
          objTimestamp = meta.imTimestamp
        }

addItemList :: (Cbor.ToCbor a) => Context -> ItemMeta -> ArchiveSiteData -> Bool -> S.Set a -> IO ()
addItemList ctx meta archive full v =
  DS.addBlob ctx.ctxDataStore (ListData (CborData (Cbor.encodeStrictBs v))) >>= \dat ->
    addSnapshot
      ctx
      (AiSiteApi meta.imSite archive)
      (StApi AstList)
      ListMeta
        { listFetch = ctx.ctxFetchId,
          listApiResponse = meta.imApiResponseId,
          listContent = LcNormal {lcContent = dat.blobId, lcFull = full},
          listApiVersion = meta.imApiVersion,
          listTimestamp = meta.imTimestamp
        }

class (Json.FromJSON a, Cbor.ToCbor (Id a), Ord (Id a)) => Item a where
  type Id a
  archiveInfo :: a -> ArchiveSiteData
  itemId :: a -> Id a
  addItemSummary :: Context -> a -> Summary -> Summary

addHtmlContent :: Context -> HtmlContent -> Summary -> Summary
addHtmlContent Context {ctxContentImages = True} (HtmlContent c) s =
  s {sumContentImages = addContentImage c (sumContentImages s)}
addHtmlContent Context {ctxContentImages = False} _ s = s

instance Item Api.Answer where
  type Id Api.Answer = AnswerId
  archiveInfo ans = AsdAnswer ans.ansAnswerId AAnsInfo
  itemId = Api.ansAnswerId
  addItemSummary ctx ans = addHtmlContent ctx (Api.ansBody ans)

instance Item Api.Badge where
  type Id Api.Badge = BadgeId
  archiveInfo b = AsdBadge b.bBadgeId ABdgInfo
  itemId = Api.bBadgeId
  addItemSummary ctx b = addHtmlContent ctx (Api.bDescription b)

instance Item Api.Collective where
  type Id Api.Collective = CollectiveSlug
  archiveInfo col = AsdCollective col.colSlug AColInfo
  itemId = Api.colSlug
  addItemSummary
    Context {ctxContentImages = True}
    Api.Collective {Api.colDescription = RawText t}
    s =
      s {sumContentImages = addContentImage t (sumContentImages s)}
  addItemSummary _ _ s = s

instance Item Api.Comment where
  type Id Api.Comment = CommentId
  archiveInfo com = AsdComment com.comCommentId AComInfo
  itemId = Api.comCommentId
  addItemSummary ctx c = addHtmlContent ctx (Api.comBody c)

instance Item Api.Question where
  type Id Api.Question = QuestionId
  archiveInfo q = AsdQuestion q.qQuestionId AQueInfo
  itemId = Api.qQuestionId
  addItemSummary ctx q = addHtmlContent ctx (Api.qBody q)

instance Item Api.Tag where
  type Id Api.Tag = TagName
  archiveInfo t = AsdTag t.tagName ATagInfo
  itemId = Api.tagName
  addItemSummary _ _ s = s

instance Item Api.TagSynonym where
  type Id Api.TagSynonym = TagName
  archiveInfo t = AsdTagSynonym t.tagSynFromTag ATSynInfo
  itemId = Api.tagSynFromTag
  addItemSummary _ _ s = s

instance Item Api.TagWiki where
  type Id Api.TagWiki = TagName
  archiveInfo t = AsdTagWiki t.twTagName ATWkInfo
  itemId = Api.twTagName
  addItemSummary ctx w = addHtmlContent ctx (Api.twBody w)

instance Item Api.User where
  type Id Api.User = UserId
  archiveInfo u = AsdUser u.usrUserId AUsrInfo
  itemId = Api.usrUserId
  addItemSummary Context {ctxProfileImages = True} Api.User {Api.usrProfileImage = LinkUrl u} s =
    s {sumProfileImages = addImageUrl u (sumProfileImages s)}
  addItemSummary Context {ctxProfileImages = False} _ s = s

addItem ::
  forall a.
  (Item a) =>
  Context ->
  Summary ->
  ItemMeta ->
  BinJson.WithBinValue a ->
  IO Summary
addItem ctx s meta (BinJson.WithBinValue bv obj) =
  addItemSummary ctx obj s
    <$ addApiItem ctx meta (archiveInfo obj) bv

addItemsResp :: forall a. (Item a) => Context -> ItemMeta -> Proxy a -> ByteString -> IO Summary
addItemsResp ctx meta _ body =
  Aeson.throwDecodeStrict body >>= \Api.Wrapper {Api.wItems = is} ->
    V.foldM' (\s v -> addItem @a ctx s meta v) mempty is

data ListArgs = ListArgs
  { lrMeta :: !ItemMeta,
    lrFull :: !Bool,
    lrResponses :: !(V.Vector HttpRequest)
  }

data SummaryWithIds i = SummaryWithIds !Summary !(S.Set i)

addItemListResp ::
  forall a.
  (Item a) =>
  Context ->
  ListArgs ->
  ArchiveSiteData ->
  Proxy a ->
  IO Summary
addItemListResp ctx (ListArgs meta full resp) archive _ = do
  SummaryWithIds s ids <-
    V.foldM'
      ( \si r ->
          Aeson.throwDecodeStrict r.hrResponse.respBody.jsonBody >>= \w ->
            V.foldM'
              ( \(SummaryWithIds s i) val@(BinJson.WithBinValue _ obj) ->
                  fmap
                    (\updateS -> SummaryWithIds updateS (S.insert (itemId obj) i))
                    (addItem @a ctx s meta val)
              )
              si
              (Api.wItems w)
      )
      (SummaryWithIds mempty S.empty)
      resp
  addItemList ctx meta archive full ids
  pure s

addRevisionSummary :: Context -> Api.Revision -> Summary -> Summary
addRevisionSummary Context {ctxContentImages = True} r initS@Summary {sumContentImages = s0} =
  initS
    { sumContentImages =
        ( addContent (Api.revBody r)
            . addContent (Api.revLastBody r)
            . addContent (Api.revComment r)
        )
          s0
    }
  where
    addContent (Just (HtmlContent c)) s = addContentImage c s
    addContent Nothing s = s
addRevisionSummary Context {ctxContentImages = False} _ s = s

data SummaryWithRev = SummaryWithRev !Summary !(M.Map RevisionId [BinJson.BinValue])

addRevisionListResp :: Context -> ListArgs -> ArchiveSiteData -> IO Summary
addRevisionListResp ctx (ListArgs meta full resp) archive =
  let SummaryWithRev summary revs =
        V.foldr'
          ( \r acc -> case Aeson.eitherDecodeStrict' r.hrResponse.respBody.jsonBody of
              Right (Api.Wrapper {Api.wItems = is}) ->
                V.foldr'
                  ( \(BinJson.WithBinValue bv obj) (SummaryWithRev s mp) ->
                      SummaryWithRev
                        (addRevisionSummary ctx obj s)
                        ( M.alter
                            ( \case
                                Just vs -> Just (bv : vs)
                                Nothing -> Just [bv]
                            )
                            (Api.revRevisionGuid obj)
                            mp
                        )
                  )
                  acc
                  is
              Left e -> throw (Aeson.AesonException e)
          )
          (SummaryWithRev mempty M.empty)
          resp
   in do
        traverse_
          (\(k, v) -> addApiItem ctx meta (AsdRevision k ARevInfo) v)
          (M.toAscList revs)
        addItemList ctx meta archive full (M.keysSet revs)
        pure summary

newtype Info = Info {infoSite :: BinJson.WithBinValue Api.Site}

Aeson.TH.deriveFromJSON
  Aeson.TH.defaultOptions {Aeson.TH.fieldLabelModifier = Aeson.camelTo2 '_' . drop 4}
  ''Info

newtype InfoItem = IInfo Info

instance Aeson.FromJSON InfoItem where
  parseJSON =
    Aeson.withArray
      "info"
      ( \v ->
          if V.length v == 1
            then IInfo <$> Aeson.parseJSON (V.head v)
            else fail "multi item in site info"
      )

addEntry :: Context -> LBS.ByteString -> IO Summary
addEntry ctx bs =
  let entry = Cbor.decodeLazyBsThrow bs
   in case arData entry of
        RdObjects {rdoType = ty, rdoResponse = resp} -> do
          apiRespId <-
            addHttpResponse ctx (arSeq entry) 0 resp >>= \hId ->
              addApiResponse
                ctx
                ApiInfo
                  { apiFetch = ctxFetchId ctx,
                    apiCallSeq = arSeq entry,
                    apiVersion = arApiVersion entry,
                    apiSite = arSite entry,
                    apiTimestamp = respTimestamp (hrResponse resp),
                    apiFilter = arFilter entry,
                    apiResponse = RdObjects {rdoType = ty, rdoResponse = hId}
                  }
          let meta =
                ItemMeta
                  { imSite = arSite entry,
                    imApiResponseId = apiRespId,
                    imApiVersion = arApiVersion entry,
                    imFilter = arFilter entry,
                    imTimestamp = respTimestamp (hrResponse resp)
                  }
              body = resp.hrResponse.respBody.jsonBody
          case ty of
            OtAnswer -> addItemsResp @Api.Answer ctx meta Proxy body
            OtBadge -> addItemsResp @Api.Badge ctx meta Proxy body
            OtCollective -> addItemsResp @Api.Collective ctx meta Proxy body
            OtComment -> addItemsResp @Api.Comment ctx meta Proxy body
            OtInfo -> do
              Api.Wrapper {Api.wItems = IInfo (Info {infoSite = BinJson.WithBinValue bv obj})} <-
                Aeson.throwDecodeStrict body
              addApiItem ctx meta AsdInfo bv
              pure
                ( if ctx.ctxProfileImages
                    then
                      Summary
                        { sumContentImages = mempty,
                          sumProfileImages =
                            let addLink (LinkUrl l) = addImageUrl l
                             in ( addLink (Api.sitIconUrl obj)
                                    . addLink (Api.sitHighResolutionIconUrl obj)
                                    . addLink (Api.sitFaviconUrl obj)
                                    . addLink (Api.sitLogoUrl obj)
                                )
                                  mempty
                        }
                    else mempty
                )
            OtQuestion -> addItemsResp @Api.Question ctx meta Proxy body
            OtRevision -> error "revision must be in api list"
            OtTag -> addItemsResp @Api.Tag ctx meta Proxy body
            OtTagSynonym -> addItemsResp @Api.TagSynonym ctx meta Proxy body
            OtTagWiki -> addItemsResp @Api.TagWiki ctx meta Proxy body
            OtUser -> addItemsResp @Api.User ctx meta Proxy body
        RdList {rdlRequest = req, rdlFull = f, rdlResponses = resp} -> do
          let timestamp = respTimestamp (hrResponse (V.head resp))
          apiRespId <-
            V.imapM (addHttpResponse ctx (arSeq entry)) resp >>= \hId ->
              addApiResponse
                ctx
                ApiInfo
                  { apiFetch = ctxFetchId ctx,
                    apiCallSeq = arSeq entry,
                    apiVersion = arApiVersion entry,
                    apiSite = arSite entry,
                    apiTimestamp = timestamp,
                    apiFilter = arFilter entry,
                    apiResponse =
                      RdList
                        { rdlRequest = req,
                          rdlFull = f,
                          rdlResponses = hId
                        }
                  }
          let meta =
                ItemMeta
                  { imSite = arSite entry,
                    imApiResponseId = apiRespId,
                    imApiVersion = arApiVersion entry,
                    imFilter = arFilter entry,
                    imTimestamp = timestamp
                  }
              args =
                ListArgs
                  { lrMeta = meta,
                    lrFull = f,
                    lrResponses = resp
                  }
          case req of
            LrAnswer {lraId = aId, lraRequest = aReq} ->
              case aReq of
                AlComment -> addItemListResp @Api.Comment ctx args (AsdAnswer aId AAnsComment) Proxy
                AlRevision -> addRevisionListResp ctx args (AsdAnswer aId AAnsRevision)
            LrCollective {lrcId = cId, lrcRequest = cReq} ->
              case cReq of
                ClAnswer -> addItemListResp @Api.Answer ctx args (AsdCollective cId AColAnswer) Proxy
                ClQuestion -> addItemListResp @Api.Question ctx args (AsdCollective cId AColQuestion) Proxy
                ClTag -> addItemListResp @Api.Tag ctx args (AsdCollective cId AColTag) Proxy
                ClUser -> addItemListResp @Api.User ctx args (AsdCollective cId AColUser) Proxy
            LrListRevision revId
              | f ->
                  let content =
                        V.concatMap
                          ( \r ->
                              case Aeson.eitherDecodeStrict r.hrResponse.respBody.jsonBody of
                                Right Api.Wrapper {Api.wItems = is} -> (is :: V.Vector (BinJson.WithBinValue Api.Revision))
                                Left e -> throw (Aeson.AesonException e)
                          )
                          resp
                   in V.foldl'
                        (\s (BinJson.WithBinValue _ r) -> addRevisionSummary ctx r s)
                        mempty
                        content
                        <$ addApiItem
                          ctx
                          meta
                          (AsdRevision revId ARevInfo)
                          (V.map (\(BinJson.WithBinValue bv _) -> bv) content)
              | otherwise -> error "incomplete revision"
            LrQuestion {lrqId = qId, lrqRequest = qReq} ->
              case qReq of
                QlAnswer -> addItemListResp @Api.Answer ctx args (AsdQuestion qId AQueAnswer) Proxy
                QlComment -> addItemListResp @Api.Comment ctx args (AsdQuestion qId AQueComment) Proxy
                QlRevision -> addRevisionListResp ctx args (AsdQuestion qId AQueRevision)
            LrTag {lrtId = tId, lrtRequest = tReq} ->
              case tReq of
                TlTagSynonym -> addItemListResp @Api.TagSynonym ctx args (AsdTag tId ATagSynonym) Proxy
            LrUser {lruId = u, lruRequest = uReq} ->
              case uReq of
                UlAnswer -> addItemListResp @Api.Answer ctx args (AsdUser u AUsrAnswer) Proxy
                UlBadge -> addItemListResp @Api.Badge ctx args (AsdUser u AUsrBadge) Proxy
                UlComment -> addItemListResp @Api.Comment ctx args (AsdUser u AUsrComment) Proxy
                UlQuestion -> addItemListResp @Api.Question ctx args (AsdUser u AUsrQuestion) Proxy

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
                      fetchId <- OS.addObject os (OtRecord (RtApiRequest RrFetch)) (Version 1 0) f.fInfo
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
