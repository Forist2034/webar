{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable, typeOf, typeRep)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import System.FilePath ((<.>), (</>))
import Test.Hspec
import Webar.Codec.Cbor
import Webar.Codec.Cbor.TH

mkTestBin :: (Show a, Eq a, ToCbor a, FromCbor a) => String -> a -> BS.ByteString -> Spec
mkTestBin name v bin =
  describe name do
    it "serialize" (encodeStrictBs v `shouldBe` bin)
    it "deserialize" do
      decodeStrictBsThrow bin `shouldBe` v

mkTest :: (Show a, Eq a, FromCbor a, ToCbor a) => String -> a -> FilePath -> Spec
mkTest name v path = do
  bin <- runIO (BS.readFile ("tests/cbor/data" </> path <.> "bin"))
  mkTestBin name v bin

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

integer :: Spec
integer = describe "integer" do
  describe "positive" do
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
  describe "negative" do
    testVal @Int8 (-1) "minus_one"
    testVal @Int16 (-10) "minus_ten"
    testVal @Int32 (-100) "-100"
    testVal @Int16 (-1000) "-1000"
  minMax
  where
    testVal ::
      (Show a, Eq a, ToCbor a, FromCbor a, Typeable a) =>
      a ->
      FilePath ->
      Spec
    testVal a = mkTest (show a ++ "::" ++ show (typeOf a)) a

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

data ProdSample = ProdSample
  { ssA :: Word32,
    ssB :: V.Vector Word64
  }
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ProdSample

prodSample :: Spec
prodSample = describe "sample" do
  mkTest
    "sample"
    ProdSample {ssA = 1, ssB = V.fromList [2, 3]}
    "prod_sample_0"
  mkTest
    "sample_bound"
    ProdSample {ssA = maxBound, ssB = V.fromList [0, 1, maxBound]}
    "prod_sample_max"

data ProdNormal = ProdNormal Int32 (V.Vector Int32) (V.Vector Word16)
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions {fieldLabelModifier = id}
  ''ProdNormal

prodNormal :: Spec
prodNormal = describe "normal" do
  mkTest
    "0"
    (ProdNormal 1 (V.fromList [2, 3]) (V.fromList [4, 5]))
    "prod_normal_0"
  mkTest
    "bound"
    (ProdNormal minBound (V.fromList [0, maxBound, 10]) (V.fromList [1, 2, 3]))
    "prod_normal_bound"

data ProdSort = ProdSort
  { psA :: Word8,
    psC :: V.Vector Int32,
    psAb :: Int32,
    psBac :: T.Text
  }
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ProdSort

prodSort :: Spec
prodSort = describe "sort" do
  mkTest
    "0"
    ProdSort
      { psC = V.fromList [-1, 0, 1],
        psBac = "example",
        psAb = 10,
        psA = maxBound
      }
    "prod_sort_0"
  mkTest
    "1"
    ProdSort {psA = 1, psC = V.empty, psAb = 12, psBac = "sss"}
    "prod_sort_1"

data ProdSortInner = ProdSortInner
  { psiB :: Word16,
    psiAa :: Bool
  }
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ProdSortInner

data ProdSortNested = ProdSortNested
  { psoI :: ProdSortInner,
    psoAa :: Word32,
    psoCab :: Int64
  }
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ProdSortNested

prodSortNested :: Spec
prodSortNested = describe "sort_nested" do
  mkTest
    "0"
    ProdSortNested
      { psoI = ProdSortInner {psiB = 10, psiAa = True},
        psoAa = 1000,
        psoCab = -6
      }
    "prod_sort_nested_0"
  mkTest
    "bound"
    ProdSortNested
      { psoI = ProdSortInner {psiB = 10, psiAa = True},
        psoCab = -255,
        psoAa = 1000
      }
    "prod_sort_nested_1"

data ProdWeird = ProdWeird
  { pwCr :: T.Text,
    pw1 :: T.Text,
    pwEuro :: T.Text,
    pwHebrew :: T.Text,
    pwControl :: T.Text,
    pwO :: T.Text,
    pwScript :: T.Text,
    pwSmiley :: T.Text
  }
  deriving (Show, Eq)

deriveProductCbor
  defaultProductOptions
    { fieldLabelModifier = \case
        "pwCr" -> "\r"
        "pw1" -> "1"
        "pwEuro" -> "€"
        "pwHebrew" -> "דּ"
        "pwControl" -> "\x80"
        "pwO" -> "ö"
        "pwScript" -> "</script>"
        "pwSmiley" -> "😂"
        _ -> error "unreachable"
    }
  ''ProdWeird

prodWeird :: Spec
prodWeird =
  mkTest
    "weird"
    ProdWeird
      { pwCr = "Carriage Return",
        pw1 = "One",
        pwEuro = "Euro Sign",
        pwHebrew = "Hebrew Letter Dalet With Dagesh",
        pwControl = "Control",
        pwO = "Latin Small Letter O With Diaeresis",
        pwScript = "Browser Challenge",
        pwSmiley = "Smiley"
      }
    "prod_weird"

data SumUnit
  = SuA
  | SuB
  | SuVar3
  deriving (Show, Eq)

deriveSumCbor
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumUnit

unitSum :: Spec
unitSum = describe "unit" do
  mkTest "a" SuA "var_a"
  mkTest "b" SuB "var_b"
  mkTest "var3" SuVar3 "var_var3"

data SumRecord
  = SrV1
      { srA :: Word32,
        srK :: Int16,
        srAb :: Int8,
        srC :: Bool
      }
  | SrSv2
      { srZ :: Bool,
        srFull :: Bool,
        srNew :: Word32
      }
  deriving (Show, Eq)

deriveSumCbor
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumRecord

recordSum :: Spec
recordSum = describe "record" do
  mkTest "v1" SrV1 {srA = 10, srK = -128, srAb = -10, srC = False} "var_v1"
  mkTest "v2" SrSv2 {srZ = False, srFull = True, srNew = 0} "var_sv2"

data SumNormal
  = SnTv1 Word8 Int16
  | SnTv2 Bool T.Text
  deriving (Show, Eq)

deriveSumCbor
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumNormal

normalSum :: Spec
normalSum = describe "normal" do
  mkTest "tv1" (SnTv1 123 (-3200)) "var_tv1"
  mkTest "tv2" (SnTv2 False "123") "var_tv2"

data SumUnary
  = StNv Bool
  | StNv2 (V.Vector Word32)
  deriving (Show, Eq)

deriveSumCbor
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumUnary

unarySum :: Spec
unarySum = describe "unary" do
  mkTest "nv1" (StNv True) "var_nv"
  mkTest "nv2" (StNv2 (V.fromList [123, 456])) "var_nv2"

data SumMixed
  = MvUnit
  | MvU2
  | MvTup Bool Bool
  | MvNewt Int8
  | MvStruct
      { mvsC :: Int8,
        mvsAc :: Bool,
        mvsAb :: T.Text
      }
  deriving (Show, Eq)

deriveSumCbor
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
      constructorTagModifier = camelTo2 '_'
    }
  ''SumMixed

mixedSum :: Spec
mixedSum = describe "mixed" do
  mkTest "unit" MvUnit "var_mv_unit"
  mkTest "unit2" MvU2 "var_mv_u2"
  mkTest "tup" (MvTup False True) "var_mv_tup"
  mkTest "unary" (MvNewt (-10)) "var_mv_newt"
  mkTest "record" MvStruct {mvsC = -1, mvsAc = True, mvsAb = "a"} "var_mv_struct"

setTests :: Spec
setTests = describe "set" do
  mkTest @(S.Set Int32) "empty" S.empty "set_empty"
  mkTest @(S.Set Int32) "example" (S.fromList [1, 2, 3]) "set_example"

mapTests :: Spec
mapTests = describe "map" do
  mkTest @(M.Map Word Bool) "empty" M.empty "map_empty"
  mkTest @(M.Map Word Bool) "example" (M.fromList [(1, True), (2, False), (3, True)]) "map_example"

uuidTests :: Spec
uuidTests = describe "uuid" do
  mkTest "nil" UUID.nil "uuid_nil"
  mkTest "1" (UUID.fromWords64 0xc2cc_10e1_57d6_4b6f 0x9899_38d9_7211_2d8c) "uuid_1"

main :: IO ()
main = hspec do
  integer
  text
  array
  bytes
  describe "bool" do
    mkTest "false" False "false"
    mkTest "true" True "true"
  describe "product" do
    prodSample
    prodNormal
    prodSort
    prodSortNested
    prodWeird
  describe "sum" do
    unitSum
    recordSum
    normalSum
    unarySum
    mixedSum
  setTests
  mapTests
  uuidTests
