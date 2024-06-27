{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((<.>), (</>))
import Test.Hspec
import Webar.Data.Cbor (FromCbor, ToCbor)
import qualified Webar.Data.Cbor as Cbor
import Webar.Data.Json (FromJSON, ToJSON)
import qualified Webar.Data.Json as Json
import Webar.Digest
import Webar.Types

mkTest ::
  (Show a, Eq a, FromCbor a, ToCbor a, FromJSON a, ToJSON a) =>
  String ->
  a ->
  FilePath ->
  Spec
mkTest name v path = describe name do
  cborBin <- runIO (BS.readFile ("tests/data/cbor/" </> path <.> "bin"))
  jsonBin <- runIO (BS.readFile ("tests/data/json/" </> path <.> "json"))

  it "serialize_cbor" (Cbor.encodeStrictBs v `shouldBe` cborBin)
  it "deserialize_cbor" do
    Cbor.decodeLazyBs (LBS.fromStrict cborBin) `shouldBe` Right (LBS.empty, v)
  it "serialize_json" (Json.encodeStrictBs v `shouldBe` jsonBin)
  it "deserialize_json" (Json.decodeStrictBs jsonBin `shouldBe` Right v)

digest :: Spec
digest = describe "Webar.Digest" do
  let emptySha256 = Sha256 (H.hash BS.empty)
  mkTest "Sha256" emptySha256 "sha256_empty"
  mkTest "Digest" (DSha256 emptySha256) "digest_sha256_empty"

miscTypes :: Spec
miscTypes = describe "Webar.Types" do
  describe "Timestamp" do
    mkTest "zero" Timestamp {tsSecs = 0, tsNanos = 0} "timestamp_0"
    mkTest -- 1980-01-02T12:34:56.1234Z
      "1980"
      Timestamp {tsSecs = 315664496, tsNanos = 123400000}
      "timestamp_1980"

main :: IO ()
main = hspec do
  digest
  miscTypes
