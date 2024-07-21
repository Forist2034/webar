{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector as V
import System.Directory.OsPath (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.OsPath.Posix
import System.OsString.Internal.Types (OsString (OsString))
import System.Posix.Directory.PosixPath
import System.Posix.Files.PosixString
import System.Posix.IO.PosixString
import System.Posix.PosixString (Fd)
import Webar.Data.BinJson (WithBinValue (..))
import qualified Webar.Data.BinJson as BinJson
import qualified Webar.Data.Cbor as Cbor
import Webar.Digest
import Webar.Server.StackExchange.Api.Object (Filter (filFilter))
import Webar.Server.StackExchange.Filter
import Webar.Server.StackExchange.Types

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
typeMapToRust :: TypeMap FilterInfo -> LTB.Builder
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
      "FilterInfo{name: \""
        <> LTB.fromText (fiName fi)
        <> "\",id: "
        <> ( case fiId fi of
               FilterId (DSha256 d) ->
                 "FilterId(Digest::Sha256(Sha256(hex!(\""
                   <> LTB.fromString (sha256ToString d)
                   <> "\"))))"
           )
        <> "}"
    field k v = "  " <> k <> ": " <> filterInfo v <> ",\n"

fWrite :: Fd -> ByteString -> IO ()
fWrite fd bs
  | BS.null bs = pure ()
  | otherwise =
      fdWrite fd bs >>= \l ->
        fWrite fd (BS.drop (fromIntegral l) bs)

addFilter :: PosixPath -> ByteString -> IO FilterInfo
addFilter root resp = do
  WithBinValue bv fil <- case Aeson.eitherDecodeStrict resp of
    Right (Wrapper v) -> pure v
    Left e -> fail ("failed to decode response: " ++ e)
  let encoded = Cbor.encodeStrictBs bv
      sha256 = sha256Hash encoded
  dir <- (root </>) <$> encodeFS (sha256ToString sha256)
  exists <- fileExist dir
  unless
    exists
    ( do
        createDirectory dir 0o755
        bracket
          (createFile (dir </> [pstr|api.bin|]) 0o444)
          closeFd
          (\fd -> fWrite fd encoded)
        setFileMode dir 0o555
    )
  pure
    FilterInfo
      { fiName = filFilter fil,
        fiId = FilterId (DSha256 sha256)
      }

main :: IO ()
main = do
  [input, root] <- getArgs
  respMap <- Cbor.decodeStrictBsThrow <$> BS.readFile input
  rootOs <- (</> [pstr|filter.sha256|]) <$> encodeFS root
  createDirectoryIfMissing True (OsString rootOs)
  infoMap <- traverse (addFilter rootOs) respMap
  LTIO.putStrLn (LTB.toLazyText (typeMapToRust infoMap))
