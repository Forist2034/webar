{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Webar.Data.BinJson (BinValue)
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json

data Op = Encode | Decode

main :: IO ()
main = do
  (op, path) <-
    getArgs >>= \case
      ["encode"] -> pure (Encode, Nothing)
      ["encode", path] -> pure (Encode, Just path)
      ["decode"] -> pure (Decode, Nothing)
      ["decode", path] -> pure (Decode, Just path)
      _ -> error "invalid args"
  input <- case path of
    Just p -> BS.readFile p
    Nothing -> BS.hGetContents stdin
  case op of
    Encode -> do
      val <- case Json.decodeStrictBs input of
        Right v -> pure (v :: BinValue)
        Left e -> hPutStrLn stderr e >> exitFailure
      LBS.hPut stdout (Cbor.encodeLazyBs val)
    Decode -> do
      val <- case Cbor.decodeStrictBs input of
        Right (_, v) -> pure (v :: BinValue)
        Left e -> hPrint stderr e >> exitFailure
      LBS.hPut stdout (Json.encodeLazyBs val)
