{-# LANGUAGE NumericUnderscores #-}

module Webar.UUID where

import qualified Basement.Block as B
import Crypto.Random.Entropy (getEntropy)
import Data.Bits
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types.Internal as U
import Data.Word
import Webar.Types (Timestamp (..))

buildV7 :: Timestamp -> Word16 -> Word64 -> UUID
buildV7 ts randA randB =
  let ms = tsSeconds ts * 1_000 + fromIntegral (tsNanoseconds ts `div` 1_000_000)
      randBH = fromIntegral (randB `unsafeShiftR` 32)
      randBL = fromIntegral (randB .&. 0xffff_ffff)
   in U.buildFromWords
        7
        (fromIntegral (ms `unsafeShiftR` 16))
        ( fromIntegral ((ms .&. 0xffff) `unsafeShiftL` 16)
            .|. fromIntegral randA
        )
        randBH
        randBL

newV7 :: Timestamp -> IO UUID
newV7 ts =
  do
    randA <- getEntropy 2
    randB <- getEntropy 8
    pure (buildV7 ts (B.index randA 0) (B.index randB 0))