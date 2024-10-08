{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.FilePath
import Webar.Blob
import Webar.Data.BinJson (WithBinValue (..))
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import Webar.Digest
import Webar.Object
import Webar.Server.StackExchange.RestApi.Filter
import Webar.Server.StackExchange.RestApi.Internal.BlobData
import Webar.Server.StackExchange.RestApi.Model (Filter (..), FilterType (..))
import Webar.Server.StackExchange.RestApi.Types
import Webar.Server.StackExchange.Source
import qualified Webar.Store.Blob.Base as DS.B
import qualified Webar.Store.Blob.WithShared as DS
import qualified Webar.Store.Object.Base as OS.B
import qualified Webar.Store.Object.Website as OS
import Webar.Types (Version (Version))

newtype Wrapper = Wrapper (BinJson.WithBinValue Filter)

instance Aeson.FromJSON Wrapper where
  parseJSON =
    Aeson.withObject
      "Wrapper"
      ( \o ->
          o Aeson..: "items"
            >>= Aeson.withArray
              "items"
              ( \v ->
                  if V.length v == 1
                    then Wrapper <$> Aeson.parseJSON (V.head v)
                    else fail "expect only one object in item"
              )
      )

-- | rust expression of TypeMap
typeMapToRust :: TypeMap FilterSpec -> LTB.Builder
typeMapToRust tm =
  mconcat
    [ "TypeMap{\n",
      field "answer" (tmAnswer tm),
      field "badge" (tmBadge tm),
      field "comment" (tmComment tm),
      field "collective" (tmCollective tm),
      field "info" (tmInfo tm),
      field "question" (tmQuestion tm),
      field "revision" (tmRevision tm),
      field "tag" (tmTag tm),
      field "tag_synonym" (tmTagSynonym tm),
      field "tag_wiki" (tmTagWiki tm),
      field "user" (tmUser tm),
      "}"
    ]
  where
    filterInfo fi =
      "filter_sha256!(\""
        <> LTB.fromText (fsName fi)
        <> "\","
        <> ( case fsId fi of
               ObjectId (DSha256 d) ->
                 "\""
                   <> LTB.fromString (sha256ToString d)
                   <> "\""
           )
        <> ")"
    field k v = "  " <> k <> ": " <> filterInfo v <> ",\n"

type ObjectStore = OS.ObjectStore () ArchiveInfo SnapshotType RecordType

addFilter :: DS.BlobStore -> ObjectStore -> ByteString -> IO FilterSpec
addFilter dataStore objStore resp = do
  WithBinValue bv fil <- case Aeson.eitherDecodeStrict resp of
    Right (Wrapper v) -> pure v
    Left e -> fail ("failed to decode response: " ++ e)
  body <- DS.addBlob dataStore (FilterData (BinJsonData (Cbor.encodeStrictBs bv)))
  filterId <-
    OS.addObject
      objStore
      (OtRecord RtFilter)
      (Version 1 0)
      FilterInfo
        { fiName = filFilter fil,
          fiSafe = case filFilterType fil of
            FtSafe -> True
            FtUnsafe -> False
            FtInvalid -> error "invalid filter",
          fiApiVersion = Api2_3,
          fiBody = DS.blobId body
        }
  pure
    FilterSpec
      { fsName = filFilter fil,
        fsId = OS.objectId filterId
      }

withDataStore :: FilePath -> FilePath -> (DS.BlobStore -> IO c) -> IO c
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
  [root, serverRoot, input] <- getArgs
  respMap <- Cbor.decodeStrictBsThrow <$> BS.readFile input
  infoMap <-
    withObjectStore
      root
      serverRoot
      ( \os ->
          withDataStore
            root
            serverRoot
            ( \ds ->
                traverse (addFilter ds os) respMap
            )
      )
  LTIO.putStrLn (LTB.toLazyText (typeMapToRust infoMap))
