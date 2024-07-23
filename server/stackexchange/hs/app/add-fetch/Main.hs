{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Bits (Bits (unsafeShiftR, (.&.)))
import qualified Data.ByteArray.Hash as MH
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Tar as Tar
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word32, Word64, Word8)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.OsPath.Posix
import qualified System.OsString.Posix as OSStr
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json
import Webar.Digest
import Webar.Fetch.Http (FetchId)
import Webar.Fetch.Http.Store (addWiresharkFetch)
import Webar.Http
import qualified Webar.Server.StackExchange.Api.Object as Api
import Webar.Server.StackExchange.Api.Source
import Webar.Server.StackExchange.Fetcher.ApiClient
import Webar.Server.StackExchange.Types
import Webar.Store.File.DedupStore (DedupStore)
import qualified Webar.Store.File.DedupStore as Files
import Webar.Store.FileSystem
import Webar.Types

buildPath :: BSB.Builder -> PosixPath
buildPath builder =
  OSStr.fromBytestring
    (BS.toStrict (BSB.toLazyByteString builder))

createHttpObject :: PosixPath -> BSB.Builder -> Sha256 -> (PosixPath -> IO ()) -> IO ()
createHttpObject root ty h act =
  let path =
        root
          </> buildPath
            ( "http/"
                <> ty
                <> ".sha256/"
                <> ( let SubDir sub _ _ _ = sha256SubDir h
                      in BSB.word8HexFixed sub
                   )
                <> "/"
                <> BSB.stringUtf8 (sha256ToString h)
            )
   in createObjectDir path (act path)

data Context = Context
  { ctxFileStore :: !DedupStore,
    ctxFetchId :: !FetchId,
    ctxRoot :: !PosixPath
  }

addHttpResponse ::
  Context ->
  Word32 ->
  Int ->
  HttpRequest ->
  IO HttpResponseId
addHttpResponse ctx callSeq respIdx req =
  let bodySha256 = DSha256 (sha256Hash (respBody (hrResponse req)))
      info =
        Cbor.encodeStrictBs
          HttpInfo
            { hiUrl = hrUrl req,
              hiFetch = ctxFetchId ctx,
              hiRequestId = hrRequestId req,
              hiCallSeq = callSeq,
              hiResponseIndex = fromIntegral respIdx,
              hiResponse = (hrResponse req) {respBody = bodySha256}
            }
      sha256 = sha256Hash info
   in HttpResponseId (DSha256 sha256)
        <$ createHttpObject
          (ctxRoot ctx)
          "request"
          sha256
          ( \path ->
              do
                createFile (path </> [pstr|info.bin|]) info
                Files.addFile
                  (ctxFileStore ctx)
                  bodySha256
                  (respBody (hrResponse req))
                  (path </> [pstr|response|])
          )

addApiResponse :: Context -> ApiInfo -> IO ApiResponseId
addApiResponse ctx resp =
  let info = Cbor.encodeStrictBs resp
      sha256 = sha256Hash info
   in ApiResponseId (DSha256 sha256)
        <$ createHttpObject
          (ctxRoot ctx)
          "api-response"
          sha256
          (\path -> createFile (path </> [pstr|info.bin|]) info)

data DataKind = ApiItem | ItemList

addObjectData ::
  DedupStore ->
  PosixPath ->
  Digest ->
  DataKind ->
  ByteString ->
  IO PosixPath
addObjectData fileStore root digest@(DSha256 sha256) kind content =
  let dir = root </> [pstr|data.sha256|] </> unsafeEncodeUtf (sha256ToString sha256)
      path =
        dir </> case kind of
          ApiItem -> [pstr|api.bin|]
          ItemList -> [pstr|list.bin|]
   in path <$ createObjectDir dir (Files.addFile fileStore digest content path)

addObjectMeta :: (Cbor.ToCbor a) => PosixPath -> PosixPath -> a -> PosixPath -> DataKind -> IO ()
addObjectMeta root snapshotId meta dataPath kind =
  let dir = root </> [pstr|snapshot|] </> snapshotId
   in createObjectDir
        dir
        ( do
            createFile (dir </> [pstr|info.bin|]) (Cbor.encodeStrictBs meta)
            createLink
              dataPath
              ( dir </> case kind of
                  ApiItem -> [pstr|api.bin|]
                  ItemList -> [pstr|list.bin|]
              )
        )

data ItemMeta = ItemMeta
  { imSite :: !ApiSiteParameter,
    imApiResponseId :: !ApiResponseId,
    imApiVersion :: !ApiVersion,
    imFilter :: !FilterId,
    imTimestamp :: !Timestamp
  }

addApiItem :: (Cbor.ToCbor a) => Context -> PosixPath -> ItemMeta -> Word32 -> a -> IO ()
addApiItem ctx root meta idx bv =
  let bin = Cbor.encodeStrictBs bv
      sha256 = sha256Hash bin
   in addObjectData (ctxFileStore ctx) root (DSha256 sha256) ApiItem bin >>= \dataPath ->
        addObjectMeta
          root
          ( buildPath
              ( "sha256-"
                  <> BSB.stringUtf8
                    ( sha256ToString
                        ( case imApiResponseId meta of
                            ApiResponseId (DSha256 s) -> s
                        )
                    )
                  <> "_"
                  <> BSB.word32Dec idx
              )
          )
          Metadata
            { metaFetch = ctxFetchId ctx,
              metaId =
                SnapshotId
                  { siApiResponse = imApiResponseId meta,
                    siIndex = Just idx
                  },
              metaContent = CNormal (DSha256 sha256),
              metaApiVersion = imApiVersion meta,
              metaFilter = imFilter meta,
              metaTimestamp = imTimestamp meta
            }
          dataPath
          ApiItem

addItemList :: (Cbor.ToCbor a) => Context -> PosixPath -> ItemMeta -> Bool -> S.Set a -> IO ()
addItemList ctx root meta full lst =
  let bin = Cbor.encodeStrictBs lst
      digest = DSha256 (sha256Hash bin)
   in addObjectData (ctxFileStore ctx) root digest ItemList bin >>= \dataPath ->
        addObjectMeta
          root
          ( unsafeEncodeUtf
              ( "sha256-"
                  ++ sha256ToString
                    ( case imApiResponseId meta of
                        ApiResponseId (DSha256 h) -> h
                    )
              )
          )
          ListMeta
            { listFetch = ctxFetchId ctx,
              listId = imApiResponseId meta,
              listContent = LcNormal {lcContent = digest, lcFull = full},
              listApiVersion = imApiVersion meta,
              listTimestamp = imTimestamp meta
            }
          dataPath
          ItemList

data IdPath = IdPath !ByteString !Word8 !BSB.Builder

idPathToPath :: ItemMeta -> IdPath -> ByteString -> PosixPath
idPathToPath meta (IdPath ty sub p) child =
  buildPath
    ( "site/"
        <> TE.encodeUtf8Builder (apiSiteParamToText (imSite meta))
        <> "/"
        <> BSB.byteString ty
        <> "/"
        <> BSB.word8HexFixed sub
        <> "/"
        <> p
        <> "/"
        <> BSB.byteString child
    )

class (Json.FromJSON a, Cbor.ToCbor (Id a), Ord (Id a)) => Item a where
  type Id a
  childDir :: Proxy a -> ByteString
  childDir _ = "info"
  idPath :: a -> IdPath
  itemId :: a -> Id a

word64Path :: ByteString -> Word64 -> IdPath
word64Path ty w = IdPath ty (fromIntegral (w .&. 0xff)) (BSB.word64Dec w)

textPath :: ByteString -> Text -> IdPath
textPath ty t =
  let bs = TE.encodeUtf8 t
      MH.SipHash h = MH.sipHash (MH.SipKey 0 0) bs
   in IdPath ty (fromIntegral (h .&. 0xff)) (BSB.byteString bs)

revisionPath :: RevisionId -> IdPath
revisionPath (RevisionId r) =
  IdPath
    "revision"
    (fromIntegral (fst (UUID.toWords64 r) `unsafeShiftR` 56))
    (BSB.byteString (UUID.toASCIIBytes r))

userPath :: UserId -> IdPath
userPath (UserId u) = IdPath "user" (fromIntegral (u .&. 0xff)) (BSB.int64Dec u)

instance Item Api.Answer where
  type Id Api.Answer = AnswerId
  idPath Api.Answer {Api.ansAnswerId = AnswerId aId} = word64Path "answer" aId
  itemId = Api.ansAnswerId

instance Item Api.Badge where
  type Id Api.Badge = BadgeId
  idPath Api.Badge {Api.bBadgeId = BadgeId bId} = word64Path "badge" bId
  itemId = Api.bBadgeId

instance Item Api.Collective where
  type Id Api.Collective = CollectiveSlug
  idPath Api.Collective {Api.colSlug = CollectiveSlug s} = textPath "collective" s
  itemId = Api.colSlug

instance Item Api.Comment where
  type Id Api.Comment = CommentId
  idPath Api.Comment {Api.comCommentId = CommentId cId} = word64Path "comment" cId
  itemId = Api.comCommentId

instance Item Api.Question where
  type Id Api.Question = QuestionId
  idPath Api.Question {Api.qQuestionId = QuestionId qId} = word64Path "question" qId
  itemId = Api.qQuestionId

instance Item Api.Tag where
  type Id Api.Tag = TagName
  idPath Api.Tag {Api.tagName = TagName t} = textPath "tag" t
  itemId = Api.tagName

instance Item Api.TagSynonym where
  type Id Api.TagSynonym = TagName
  idPath Api.TagSynonym {Api.tagSynFromTag = TagName t} = textPath "tag_synonym" t
  itemId = Api.tagSynFromTag

instance Item Api.TagWiki where
  type Id Api.TagWiki = TagName
  childDir _ = "wiki"
  idPath Api.TagWiki {Api.twTagName = TagName t} = textPath "tag" t
  itemId = Api.twTagName

instance Item Api.User where
  type Id Api.User = UserId
  idPath u = userPath (Api.usrUserId u)
  itemId = Api.usrUserId

addItem :: forall a. (Item a) => Context -> ItemMeta -> Word32 -> BinJson.WithBinValue a -> IO ()
addItem ctx meta idx (BinJson.WithBinValue bv obj) =
  addApiItem
    ctx
    (ctxRoot ctx </> idPathToPath meta (idPath obj) (childDir @a Proxy))
    meta
    idx
    bv

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
  IdPath ->
  ByteString ->
  Proxy a ->
  IO ()
addItemListResp ctx (ListArgs meta full resp) p child _ = do
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
  addItemList
    ctx
    (ctxRoot ctx </> idPathToPath meta p child)
    meta
    full
    ids

addRevisionListResp :: Context -> ListArgs -> IdPath -> IO ()
addRevisionListResp ctx (ListArgs meta full resp) p =
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
          ( \idx (k, v) ->
              addApiItem
                ctx
                (ctxRoot ctx </> idPathToPath meta (revisionPath k) "info")
                meta
                idx
                v
          )
          [0 ..]
          (M.toAscList revs)
        addItemList
          ctx
          (ctxRoot ctx </> idPathToPath meta p "revision")
          meta
          full
          (M.keysSet revs)

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
              addApiItem
                ctx
                ( ctxRoot ctx
                    </> buildPath
                      ( "site/"
                          <> TE.encodeUtf8Builder (apiSiteParamToText (arSite entry))
                          <> "/info"
                      )
                )
                meta
                0
                bv
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
            LrAnswer {lraId = AnswerId aId, lraRequest = aReq} ->
              let p = word64Path "answer" aId
               in case aReq of
                    AlComment -> addItemListResp @Api.Comment ctx args p "comment" Proxy
                    AlRevision -> addRevisionListResp ctx args p
            LrCollective {lrcId = CollectiveSlug cId, lrcRequest = cReq} ->
              let p = textPath "collective" cId
               in case cReq of
                    ClAnswer -> addItemListResp @Api.Answer ctx args p "answer" Proxy
                    ClQuestion -> addItemListResp @Api.Question ctx args p "question" Proxy
                    ClTag -> addItemListResp @Api.Tag ctx args p "tag" Proxy
                    ClUser -> addItemListResp @Api.User ctx args p "user" Proxy
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
                   in addApiItem
                        ctx
                        (ctxRoot ctx </> idPathToPath meta (revisionPath revId) "info")
                        meta
                        0
                        content
              | otherwise -> error "incomplete revision"
            LrQuestion {lrqId = QuestionId qId, lrqRequest = qReq} ->
              let p = word64Path "question" qId
               in case qReq of
                    QlAnswer -> addItemListResp @Api.Answer ctx args p "answer" Proxy
                    QlComment -> addItemListResp @Api.Comment ctx args p "comment" Proxy
                    QlRevision -> addRevisionListResp ctx args p
            LrTag {lrtId = TagName tId, lrtRequest = tReq} ->
              let p = textPath "tag" tId
               in case tReq of
                    TlTagSynonym -> addItemListResp @Api.TagSynonym ctx args p "synonym" Proxy
            LrUser {lruId = u, lruRequest = uReq} ->
              let p = userPath u
               in case uReq of
                    UlAnswer -> addItemListResp @Api.Answer ctx args p "answer" Proxy
                    UlBadge -> addItemListResp @Api.Badge ctx args p "badge" Proxy
                    UlComment -> addItemListResp @Api.Comment ctx args p "comment" Proxy
                    UlQuestion -> addItemListResp @Api.Question ctx args p "question" Proxy

main :: IO ()
main = do
  (storeRoot, fileStore, fetchPath) <-
    getArgs >>= \case
      [sr, fs, fp] -> pure (sr, fs, fp)
      _ -> error "invalid args"
  root <- encodeFS storeRoot
  fetchP <- encodeFS fetchPath
  fetchId <- addWiresharkFetch @Void root Proxy fetchP
  dedupStore <- encodeFS fileStore >>= Files.openOrCreate
  let context =
        Context
          { ctxFileStore = dedupStore,
            ctxFetchId = fetchId,
            ctxRoot = root
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
