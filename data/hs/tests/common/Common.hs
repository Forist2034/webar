{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common
  ( DataTests,
    ProdSample,
    prodSample,
    ProdNormal,
    prodNormal,
    ProdSort,
    prodSort,
    ProdSortNested,
    prodSortNested,
    SumUnit,
    unitSum,
    SumNormal,
    normalSum,
    SumRecord,
    recordSum,
    SumUnary,
    unarySum,
    SumMixed,
    mixedSum,
    uuidTests,
  )
where

import Data.Int
import Data.Text (Text)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Webar.Data.TH

type DataTests a = (String -> a -> FilePath -> Spec) -> Spec

data ProdSample = ProdSample
  { ssA :: Word32,
    ssB :: V.Vector Word64
  }
  deriving (Show, Eq)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ProdSample

prodSample :: DataTests ProdSample
prodSample mkTest = describe "sample" do
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

deriveProdData
  ProductOptions {fieldLabelModifier = id}
  ''ProdNormal

prodNormal :: DataTests ProdNormal
prodNormal mkTest = describe "normal" do
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
    psBac :: Text
  }
  deriving (Show, Eq)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ProdSort

prodSort :: DataTests ProdSort
prodSort mkTest = describe "sort" do
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

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ProdSortInner

data ProdSortNested = ProdSortNested
  { psoI :: ProdSortInner,
    psoAa :: Word32,
    psoCab :: Int64
  }
  deriving (Show, Eq)

deriveProdData
  ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ProdSortNested

prodSortNested :: DataTests ProdSortNested
prodSortNested mkTest = describe "sort_nested" do
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

data SumUnit
  = SuA
  | SuB
  | SuVar3
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumUnit

unitSum :: DataTests SumUnit
unitSum mkTest = describe "unit" do
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

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumRecord

recordSum :: DataTests SumRecord
recordSum mkTest = describe "record" do
  mkTest "v1" SrV1 {srA = 10, srK = -128, srAb = -10, srC = False} "var_v1"
  mkTest "v2" SrSv2 {srZ = False, srFull = True, srNew = 0} "var_sv2"

data SumNormal
  = SnTv1 Word8 Int16
  | SnTv2 Bool Text
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumNormal

normalSum :: DataTests SumNormal
normalSum mkTest = describe "normal" do
  mkTest "tv1" (SnTv1 123 (-3200)) "var_tv1"
  mkTest "tv2" (SnTv2 False "123") "var_tv2"

data SumUnary
  = StNv Bool
  | StNv2 (V.Vector Word32)
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumUnary

unarySum :: DataTests SumUnary
unarySum mkTest = describe "unary" do
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
        mvsAb :: Text
      }
  deriving (Show, Eq)

deriveSumData
  SumOptions
    { sumProduct = ProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
      constructorTagModifier = camelTo2 '_'
    }
  ''SumMixed

mixedSum :: DataTests SumMixed
mixedSum mkTest = describe "mixed" do
  mkTest "unit" MvUnit "var_mv_unit"
  mkTest "unit2" MvU2 "var_mv_u2"
  mkTest "tup" (MvTup False True) "var_mv_tup"
  mkTest "unary" (MvNewt (-10)) "var_mv_newt"
  mkTest "record" MvStruct {mvsC = -1, mvsAc = True, mvsAb = "a"} "var_mv_struct"

uuidTests :: DataTests UUID.UUID
uuidTests mkTest = describe "uuid" do
  mkTest "nil" UUID.nil "uuid_nil"
  mkTest "1" (UUID.fromWords64 0xc2cc10e157d64b6f 0x989938d972112d8c) "uuid_1"