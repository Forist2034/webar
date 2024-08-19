{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Common
import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import qualified Data.Vector as V
import Data.Word
import System.FilePath ((<.>), (</>))
import Test.Hspec
import Webar.Data.Cbor

mkTestBin :: (Show a, Eq a, ToCbor a, FromCbor a) => String -> a -> BS.ByteString -> Spec
mkTestBin name v bin =
  describe name do
    it "serialize" (encodeStrictBs v `shouldBe` bin)
    it "deserialize" do
      decodeLazyBs (LBS.fromStrict bin) `shouldBe` Right (LBS.empty, v)

mkTest :: (Show a, Eq a, FromCbor a, ToCbor a) => String -> a -> FilePath -> Spec
mkTest name v path = do
  bin <- runIO (BS.readFile ("tests/cbor/data" </> path <.> "bin"))
  mkTestBin name v bin

testVal ::
  (Show a, Eq a, ToCbor a, FromCbor a, Typeable a) =>
  a ->
  FilePath ->
  Spec
testVal a = mkTest (show a ++ "::" ++ show (typeOf a)) a

positive :: Spec
positive = describe "positive" do
  testVal @Word8 0 "zero"
  testVal @Word64 0 "zero"
  testVal @Word32 1 "one"
  testVal @Int32 10 "ten"
  testVal @Word8 23 "23"
  testVal @Word64 24 "24"
  testVal @Int8 100 "100"
  testVal @Word16 1000 "1000"
  testVal @Word32 1_000_000 "million"
  testVal @Int64 1_000_000_000_000 "1e12"

negative :: Spec
negative = describe "negative" do
  testVal @Int8 (-1) "minus_one"
  testVal @Int16 (-10) "minus_ten"
  testVal @Int32 (-100) "-100"
  testVal @Int16 (-1000) "-1000"

minMax :: Spec
minMax = describe "min_max" do
  testInt @Int64 Proxy
  testWord @Word64 Proxy
  testInt @Int8 Proxy
  testWord @Word8 Proxy
  testInt @Int16 Proxy
  testWord @Word16 Proxy
  testInt @Int32 Proxy
  testWord @Word32 Proxy
  where
    testInt ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, ToCbor a, FromCbor a, Typeable a) =>
      Proxy a ->
      Spec
    testInt p = describe (show (typeRep p)) do
      let tName = "i" ++ show (finiteBitSize (minBound @a))
      mkTest "min" (minBound @a) (tName ++ "_min")
      mkTest "max" (maxBound @a) (tName ++ "_max")

    testWord ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, ToCbor a, FromCbor a, Typeable a) =>
      Proxy a ->
      Spec
    testWord p =
      mkTest
        (show (typeRep p))
        (maxBound @a)
        ("u" ++ show (finiteBitSize (maxBound @a)) ++ "_max")

text :: Spec
text = describe "text" do
  test "" "empty_str"
  test "a" "a"
  test "IETF" "ietf"
  test "\"\\" "escape_str"
  test "ü" "u_str"
  test "水" "ch_str"
  test "𐅑" "geek_str"

  large <- runIO (TE.decodeUtf8 <$> BS.readFile "tests/cbor/data/large_text.txt")
  mkTest "large" large "large_text"
  where
    test :: T.Text -> FilePath -> Spec
    test v = mkTest (show v) v

array :: Spec
array = describe "array" do
  mkTest "empty" (V.empty @Word8) "empty_array"
  mkTest "123" (V.fromList @Word32 [1, 2, 3]) "123_array"
  mkTest "25 elem" (V.fromList @Int32 [1 .. 25]) "array_25"

bytes :: Spec
bytes = describe "bytes" do
  mkTest "empty" BS.empty "empty_bytes"
  mkTest "sample" (BS.pack [0x01, 0x02, 0x03, 0x04]) "sample_bytes"

main :: IO ()
main = hspec do
  positive
  negative
  minMax
  mkTest "false" False "false"
  mkTest "true" True "true"
  text
  array
  bytes
  describe "product" do
    prodSample mkTest
    prodNormal mkTest
    prodSort mkTest
    prodSortNested mkTest
    prodUnsorted mkTest
  describe "sum" do
    unitSum mkTest
    recordSum mkTest
    normalSum mkTest
    unarySum mkTest
    unsortedSum mkTest
    mixedSum mkTest
  uuidTests mkTest
  setTests mkTest
  mapTests mkTest