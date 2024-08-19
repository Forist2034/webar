{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
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
    ProdUnsorted,
    prodUnsorted,
    ProdWeird,
    prodWeird,
    SumUnit,
    unitSum,
    SumNormal,
    normalSum,
    SumRecord,
    recordSum,
    SumUnary,
    unarySum,
    SumUnsorted,
    unsortedSum,
    SumMixed,
    mixedSum,
    setTests,
    mapTests,
    uuidTests,
  )
where

import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
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
  defaultProductOptions {fieldLabelModifier = id}
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
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
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
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''ProdSortInner

data ProdSortNested = ProdSortNested
  { psoI :: ProdSortInner,
    psoAa :: Word32,
    psoCab :: Int64
  }
  deriving (Show, Eq)

deriveProdData
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
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

data ProdUnsorted = ProdUnsorted
  { psuB :: Word8,
    psuA :: Bool,
    psuCcc :: Int32
  }
  deriving (Show, Eq)

deriveProdData
  defaultProductOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3,
      sortFields = False
    }
  ''ProdUnsorted

prodUnsorted :: DataTests ProdUnsorted
prodUnsorted mkTest =
  mkTest
    "unsorted"
    ProdUnsorted
      { psuB = 10,
        psuA = False,
        psuCcc = -32768
      }
    "prod_unsorted_0"

data ProdWeird = ProdWeird
  { pwCr :: Text,
    pw1 :: Text,
    pwEuro :: Text,
    pwHebrew :: Text,
    pwControl :: Text,
    pwO :: Text,
    pwScript :: Text,
    pwSmiley :: Text
  }
  deriving (Show, Eq)

deriveProdData
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

prodWeird :: DataTests ProdWeird
prodWeird mkTest =
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

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_' . drop 2
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
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2},
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
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = id},
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
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = id},
      constructorTagModifier = camelTo2 '_' . drop 2
    }
  ''SumUnary

unarySum :: DataTests SumUnary
unarySum mkTest = describe "unary" do
  mkTest "nv1" (StNv True) "var_nv"
  mkTest "nv2" (StNv2 (V.fromList [123, 456])) "var_nv2"

data SumUnsorted
  = SusUnit
  | SusUnary Word8
  | SusRecord1
      { suR1S :: Text,
        suR1v :: Bool,
        suR1c :: Word8
      }
  | SusTuple Int8 Word16
  | SusRecord2
      { suR2C :: Int8,
        suR2B :: Bool,
        suR2Cc :: Word16
      }
  deriving (Show, Eq)

deriveSumData
  defaultSumOptions
    { constructorTagModifier = camelTo2 '_',
      sumProduct =
        defaultProductOptions
          { fieldLabelModifier = camelTo2 '_' . drop 4,
            sortFields = False
          }
    }
  ''SumUnsorted

unsortedSum :: DataTests SumUnsorted
unsortedSum mkTest = describe "unsorted" do
  mkTest "unit" SusUnit "var_sus_unit"
  mkTest "unary" (SusUnary 255) "var_sus_unary"
  mkTest
    "record1"
    SusRecord1 {suR1S = "server", suR1v = True, suR1c = 0}
    "var_sus_record1"
  mkTest "tuple" (SusTuple (-128) 65535) "var_sus_tuple"
  mkTest
    "record2"
    SusRecord2 {suR2C = -128, suR2B = True, suR2Cc = 65535}
    "var_sus_record2"

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
  defaultSumOptions
    { sumProduct = defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 3},
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

setTests :: DataTests (S.Set Int32)
setTests mkTest = describe "set" do
  mkTest "empty" S.empty "set_empty"
  mkTest "example" (S.fromList [1, 2, 3]) "set_example"

mapTests :: DataTests (M.Map Word8 Bool)
mapTests mkTest = describe "map" do
  mkTest "empty" M.empty "map_empty"
  mkTest "example" (M.fromList [(1, True), (2, False), (3, True)]) "map_example"

uuidTests :: DataTests UUID.UUID
uuidTests mkTest = describe "uuid" do
  mkTest "nil" UUID.nil "uuid_nil"
  mkTest "1" (UUID.fromWords64 0xc2cc10e157d64b6f 0x989938d972112d8c) "uuid_1"