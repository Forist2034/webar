{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified Data.UUID.Types as UUID
import Test.Hspec
import Webar.Types
import Webar.UUID

uuidTests :: Spec
uuidTests = do
  describe "pack uuid v7" do
    let test name ts (ra, rb) (wh, wl) =
          it name (buildV7 ts ra rb `shouldBe` UUID.fromWords64 wh wl)
    test
      "packs null"
      (Timestamp 0 0)
      (0, 0)
      (0x0000000000007000, 0x8000000000000000)
    test
      "packs timestamp"
      (Timestamp 1234567890 123456)
      (123, 456789)
      (0x011f71fb0450707b, 0x800000000006f855)

main :: IO ()
main = hspec do
  describe "uuid" uuidTests