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
import Control.Monad (zipWithM_)
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
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Word (Word32)
import System.Environment (getArgs)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json
import Webar.Fetch.Http.Store (addWiresharkFetch)
import Webar.Http
import Webar.Object
import Webar.Server.StackExchange.Api.Filter (FilterId)
import qualified Webar.Server.StackExchange.Api.Model as Api
import Webar.Server.StackExchange.Api.Request
import Webar.Server.StackExchange.Api.Source
import Webar.Server.StackExchange.Api.Types
import Webar.Server.StackExchange.Fetcher.ApiClient
import Webar.Server.StackExchange.Source
import qualified Webar.Store.Data.Base as DS.B
import Webar.Store.Data.WithShared (DataStore)
import qualified Webar.Store.Data.WithShared as DS
import qualified Webar.Store.Object.Base as OS.B
import qualified Webar.Store.Object.Website as OS
import Webar.Types (Timestamp)

type ObjectStore = OS.ObjectStore () ArchiveInfo SnapshotType RecordType

data Context = Context
  { ctxDataStore :: !DataStore,
    ctxObjectStore :: !ObjectStore,
    ctxFetchId :: !FetchId
  }

addHttpResponse ::
  Context ->
  Word32 ->
  Int ->
  HttpRequest ->
  IO HttpResponseId
addHttpResponse ctx callSeq respIdx req =
  DS.addByteString ctx.ctxDataStore req.hrResponse.respBody >>= \body ->
    OS.objectId
      <$> OS.addObject
        ctx.ctxObjectStore
        (OtRecord (RtApiRequest RrHttpRequest))
        1
        HttpInfo
          { hiUrl = hrUrl req,
            hiFetch = ctxFetchId ctx,
            hiRequestId = hrRequestId req,
            hiCallSeq = callSeq,
            hiResponseIndex = fromIntegral respIdx,
            hiResponse = (hrResponse req) {respBody = body.dataId}
          }

addApiResponse :: Context -> ApiInfo -> IO ApiResponseId
addApiResponse ctx resp =
  OS.objectId
    <$> OS.addObject ctx.ctxObjectStore (OtRecord (RtApiRequest RrApiResponse)) 1 resp

addSnapshot :: (Cbor.ToCbor a) => Context -> ArchiveInfo -> SnapshotType -> a -> IO ()
addSnapshot ctx archive ty meta =
  OS.addObject ctx.ctxObjectStore OtArchive 1 archive >>= \arch ->
    void (OS.addObject ctx.ctxObjectStore (OtSnapshot arch.objectId ty) 1 meta)

data ItemMeta = ItemMeta
  { imSite :: !ApiSiteParameter,
    imApiResponseId :: !ApiResponseId,
    imApiVersion :: !ApiVersion,
    imFilter :: !FilterId,
    imTimestamp :: !Timestamp
  }

addApiItem :: (Cbor.ToCbor a) => Context -> ItemMeta -> ArchiveSiteData -> Word32 -> a -> IO ()
addApiItem ctx meta archive idx bv =
  DS.addByteString ctx.ctxDataStore (Cbor.encodeStrictBs bv) >>= \dat ->
    addSnapshot
      ctx
      (AiSiteApi meta.imSite archive)
      (StApi AstObject)
      ObjectMeta
        { objFetch = ctx.ctxFetchId,
          objApiResponse = meta.imApiResponseId,
          objApiIndex = Just idx,
          objContent = CNormal dat.dataId,
          objApiVersion = meta.imApiVersion,
          objFilter = meta.imFilter,
          objTimestamp = meta.imTimestamp
        }

addItemList :: (Cbor.ToCbor a) => Context -> ItemMeta -> ArchiveSiteData -> Bool -> S.Set a -> IO ()
addItemList ctx meta archive full v =
  DS.addByteString ctx.ctxDataStore (Cbor.encodeStrictBs v) >>= \dat ->
    addSnapshot
      ctx
      (AiSiteApi meta.imSite archive)
      (StApi AstList)
      ListMeta
        { listFetch = ctx.ctxFetchId,
          listApiResponse = meta.imApiResponseId,
          listContent = LcNormal {lcContent = dat.dataId, lcFull = full},
          listApiVersion = meta.imApiVersion,
          listTimestamp = meta.imTimestamp
        }

class (Json.FromJSON a, Cbor.ToCbor (Id a), Ord (Id a)) => Item a where
  type Id a
  archiveInfo :: a -> ArchiveSiteData
  itemId :: a -> Id a

instance Item Api.Answer where
  type Id Api.Answer = AnswerId
  archiveInfo ans = AsdAnswer ans.ansAnswerId AAnsInfo
  itemId = Api.ansAnswerId

instance Item Api.Badge where
  type Id Api.Badge = BadgeId
  archiveInfo b = AsdBadge b.bBadgeId ABdgInfo
  itemId = Api.bBadgeId

instance Item Api.Collective where
  type Id Api.Collective = CollectiveSlug
  archiveInfo col = AsdCollective col.colSlug AColInfo
  itemId = Api.colSlug

instance Item Api.Comment where
  type Id Api.Comment = CommentId
  archiveInfo com = AsdComment com.comCommentId AComInfo
  itemId = Api.comCommentId

instance Item Api.Question where
  type Id Api.Question = QuestionId
  archiveInfo q = AsdQuestion q.qQuestionId AQueInfo
  itemId = Api.qQuestionId

instance Item Api.Tag where
  type Id Api.Tag = TagName
  archiveInfo t = AsdTag t.tagName ATagInfo
  itemId = Api.tagName

instance Item Api.TagSynonym where
  type Id Api.TagSynonym = TagName
  archiveInfo t = AsdTagSynonym t.tagSynFromTag ATSynInfo
  itemId = Api.tagSynFromTag

instance Item Api.TagWiki where
  type Id Api.TagWiki = TagName
  archiveInfo t = AsdTagWiki t.twTagName ATWkInfo
  itemId = Api.twTagName

instance Item Api.User where
  type Id Api.User = UserId
  archiveInfo u = AsdUser u.usrUserId AUsrInfo
  itemId = Api.usrUserId

addItem :: forall a. (Item a) => Context -> ItemMeta -> Word32 -> BinJson.WithBinValue a -> IO ()
addItem ctx meta idx (BinJson.WithBinValue bv obj) =
  addApiItem ctx meta (archiveInfo obj) idx bv

addItemsResp :: forall a. (Item a) => Context -> ItemMeta -> Proxy a -> ByteString -> IO ()
addItemsResp ctx meta _ body =
  Aeson.throwDecodeStrict body >>= \Api.Wrapper {Api.wItems = is} ->
    V.imapM_ (\idx v -> addItem @a ctx meta (fromIntegral idx) v) is

data ListArgs = ListArgs
  { lrMeta :: !ItemMeta,
    lrFull :: !Bool,
    lrResponses :: !(V.Vector HttpRequest)
  }

addItemListResp ::
  forall a.
  (Item a) =>
  Context ->
  ListArgs ->
  ArchiveSiteData ->
  Proxy a ->
  IO ()
addItemListResp ctx (ListArgs meta full resp) archive _ = do
  ids <-
    V.foldM'
      ( \i r ->
          let startIdx = S.size i
           in Aeson.throwDecodeStrict (respBody (hrResponse r)) >>= \w ->
                V.ifoldM'
                  ( \acc objIdx val@(BinJson.WithBinValue _ obj) ->
                      S.insert (itemId obj) acc
                        <$ addItem @a ctx meta (fromIntegral (startIdx + objIdx)) val
                  )
                  i
                  (Api.wItems w)
      )
      S.empty
      resp
  addItemList ctx meta archive full ids

addRevisionListResp :: Context -> ListArgs -> ArchiveSiteData -> IO ()
addRevisionListResp ctx (ListArgs meta full resp) archive =
  let revs =
        V.foldr'
          ( \r mp -> case Aeson.eitherDecodeStrict' (respBody (hrResponse r)) of
              Right (Api.Wrapper {Api.wItems = is}) ->
                V.foldr'
                  ( \(BinJson.WithBinValue bv obj) ->
                      M.alter
                        ( \case
                            Just vs -> Just (bv : vs)
                            Nothing -> Just [bv]
                        )
                        (Api.revRevisionGuid obj)
                  )
                  mp
                  is
              Left e -> throw (Aeson.AesonException e)
          )
          M.empty
          resp
   in do
        zipWithM_
          (\idx (k, v) -> addApiItem ctx meta (AsdRevision k ARevInfo) idx v)
          [0 ..]
          (M.toAscList revs)
        addItemList ctx meta archive full (M.keysSet revs)

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

addEntry :: Context -> LBS.ByteString -> IO ()
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
              body = respBody (hrResponse resp)
          case ty of
            OtAnswer -> addItemsResp @Api.Answer ctx meta Proxy body
            OtBadge -> addItemsResp @Api.Badge ctx meta Proxy body
            OtCollective -> addItemsResp @Api.Collective ctx meta Proxy body
            OtComment -> addItemsResp @Api.Comment ctx meta Proxy body
            OtInfo -> do
              Api.Wrapper {Api.wItems = IInfo (Info {infoSite = BinJson.WithBinValue bv _})} <-
                Aeson.throwDecodeStrict body
              addApiItem ctx meta AsdInfo 0 bv
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
                              case Aeson.eitherDecodeStrict (respBody (hrResponse r)) of
                                Right Api.Wrapper {Api.wItems = is} -> (is :: V.Vector BinJson.BinValue)
                                Left e -> throw (Aeson.AesonException e)
                          )
                          resp
                   in addApiItem ctx meta (AsdRevision revId ARevInfo) 0 content
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

withDataStore :: FilePath -> FilePath -> (DataStore -> IO c) -> IO c
withDataStore root serverRoot f =
  bracket
    (DS.B.openByFilePath (root </> "store/data"))
    DS.B.close
    ( \baseData ->
        bracket
          (DS.openByFilePath baseData (serverRoot </> "store/data"))
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
  (storeRoot, serverRoot, fetchPath) <-
    getArgs >>= \case
      [sr, fs, fp] -> pure (sr, fs, fp)
      _ -> error "invalid args"
  withDataStore
    storeRoot
    serverRoot
    ( \ds ->
        withObjectStore
          storeRoot
          serverRoot
          ( \os -> do
              fetchId <- addWiresharkFetch os ds (RtApiRequest RrFetch) fetchPath
              let context =
                    Context
                      { ctxDataStore = ds,
                        ctxObjectStore = os,
                        ctxFetchId = fetchId
                      }
              C.withSourceFile
                (fetchPath ++ "/data.tar")
                ( \i ->
                    C.runConduit
                      ( i
                          .| Tar.untar
                            ( \fi ->
                                C.map BSB.byteString .| C.sinkLazyBuilder >>= \ent ->
                                  liftIO
                                    ( catch
                                        (addEntry context ent >> print (Tar.filePath fi))
                                        ( \(SomeException exn) ->
                                            hPutStrLn
                                              stderr
                                              (show (Tar.filePath fi) ++ ": " ++ displayException exn)
                                        )
                                    )
                            )
                      )
                )
          )
    )
