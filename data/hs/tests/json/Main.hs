{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Common
import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.ByteString as BS
import Data.Int
import Data.Text (Text)
import Data.Typeable
import qualified Data.Vector as V
import Data.Word
import System.FilePath
import Test.Hspec
import Webar.Data.Json

mkTestBin ::
  (Show a, Eq a, FromJSON a, ToJSON a) =>
  String ->
  a ->
  BS.ByteString ->
  Spec
mkTestBin name v bin =
  describe name do
    it "serialize" (encodeStrictBs v `shouldBe` bin)
    it "deserialize" (decodeStrictBs bin `shouldBe` Right v)

mkTest :: (Show a, Eq a, FromJSON a, ToJSON a) => String -> a -> FilePath -> Spec
mkTest name v path = do
  bin <- runIO (BS.readFile ("tests/json/data" </> path <.> "json"))
  mkTestBin name v bin

minMax :: Spec
minMax = describe "min_max" do
  testInt @Int8 Proxy
  testWord @Word8 Proxy
  testInt @Int16 Proxy
  testWord @Word16 Proxy
  testInt @Int32 Proxy
  testWord @Word32 Proxy
  testInt @Int64 Proxy
  testWord @Word64 Proxy
  where
    testWord ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, Typeable a, FromJSON a, ToJSON a) =>
      Proxy a ->
      Spec
    testWord p =
      mkTest
        (show (typeRep p))
        (maxBound @a)
        ("u" ++ show (finiteBitSize (maxBound @a)) ++ "_max")
    testInt ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, Typeable a, FromJSON a, ToJSON a) =>
      Proxy a ->
      Spec
    testInt p =
      describe (show (typeRep p)) do
        let tName = "i" ++ show (finiteBitSize (maxBound @a))
        mkTest "max" (maxBound @a) (tName ++ "_max")
        mkTest "min" (minBound @a) (tName ++ "_min")

text :: Spec
text = describe "text" do
  test "" "empty_str"
  test "example string" "str"
  test "Å" "unnormalized_str"
  test "水" "ch_str"
  test "𐅑" "geek_str"
  test "€$\x0f\nA'B\"\\\\\"/" "escape_str"
  where
    test :: Text -> FilePath -> Spec
    test t = mkTest (show t) t

array :: Spec
array = describe "array" do
  mkTest "empty" (V.empty @Int32) "empty_arr"
  mkTest "array 123" (V.fromList @Word32 [1, 2, 3]) "123_arr"

main :: IO ()
main = hspec do
  minMax
  text
  array
  describe "product" do
    prodSample mkTest
    prodNormal mkTest
    prodSort mkTest
    prodSortNested mkTest
    prodWeird mkTest
  describe "sum" do
    unitSum mkTest
    recordSum mkTest
    normalSum mkTest
    unarySum mkTest
    mixedSum mkTest
  uuidTests mkTest
  setTests mkTest
  mapTests mkTest
