{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Data.Json.TH
  ( ProductOptions (..),
    SumOptions (..),
    camelTo2,
    mkSumToJSON,
    mkProdToJSON,
    deriveSumToJSON,
    deriveProdToJSON,
    mkProdParseJSON,
    mkSumParseJSON,
    deriveProdFromJSON,
    deriveSumFromJSON,
    deriveProdJSON,
    deriveSumJSON,
  )
where

import Data.Aeson ((.:))
import qualified Data.Aeson as AE
import Data.Aeson.Encoding
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ((<?>))
import qualified Data.Aeson.Types as AE
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Webar.Data.Internal.TH
import Webar.Data.Json

serializer :: Serializer
serializer =
  Serializer
    { serField = \e -> [|toJson $(pure e)|],
      serNamedField = \k v -> [|pair $(lift (Key.fromText k)) (toJson $(pure v))|],
      serUnitSum = \v -> [|text $(lift v)|],
      serUnarySum = \c v ->
        [|pairs (pair $(lift (Key.fromText c)) (toJson $(pure v)))|],
      serNormalSum = \c fs ->
        [|pairs (pair $(lift (Key.fromText c)) $(listExpr (NEL.toList fs)))|],
      serRecSum = \c fs ->
        [|pairs (pair $(lift (Key.fromText c)) $(recordExpr fs))|],
      serRecProd = recordExpr,
      serNormalProd = listExpr . NEL.toList
    }
  where
    recordExpr (h :| t) =
      let contentExpr = foldl' (\e v -> InfixE (Just e) (VarE '(<>)) (Just v)) h t
       in [|pairs $(pure contentExpr)|]
    listExpr fs = [|list id $(pure (ListE fs))|]

serializeClass :: Class
serializeClass = Class {clsName = ''ToJSON, clsFun = 'toJson}

mkSumToJSON :: SumOptions -> Name -> ExpQ
mkSumToJSON = mkSerializeSum serializer

mkProdToJSON :: ProductOptions -> Name -> ExpQ
mkProdToJSON = mkSerializeProd serializer

deriveSumToJSON :: SumOptions -> Name -> DecsQ
deriveSumToJSON = deriveSerializeSum serializeClass serializer

deriveProdToJSON :: ProductOptions -> Name -> DecsQ
deriveProdToJSON = deriveSerializeProd serializeClass serializer

parseArrEntry :: (FromJSON a) => V.Vector AE.Value -> Int -> AE.Parser a
parseArrEntry v idx =
  parseJSON (V.unsafeIndex v idx) <?> AE.Index idx
{-# INLINE parseArrEntry #-}

parseField :: (FromJSON a) => AE.Object -> AE.Key -> AE.Parser a
parseField o k = o .: k <?> AE.Key k
{-# INLINE parseField #-}

deserializer :: Deserializer Exp Exp Exp
deserializer =
  Deserializer
    { desField = \vec idx ->
        [|parseArrEntry $(pure vec) $(lift idx)|],
      desNamedField = \obj keyStr ->
        [|parseField $(pure obj) $(lift (Key.fromText keyStr))|],
      desNormalProd = \ci f -> desArray (ciType ci) f,
      desRecProd = \ci f -> desObject (ciType ci) f,
      desUnaryCon = \v ci ->
        [|$(conE (ciCon ci)) <$> parseJSON $(pure v)|],
      desNormalCon = \v ci f ->
        desArray (ciCon ci) f `appE` pure v,
      desRecCon = \v ci f ->
        desObject (ciCon ci) f `appE` pure v,
      desUnitSum = \ty cs -> [|AE.withText $(lift (show ty)) $(cs TeArg)|],
      desFieldSum = \ty cs -> do
        obj <- newName "obj"
        body <- fieldSum ty cs obj
        [|AE.withObject $(lift (show ty)) $(pure (LamE [VarP obj] body))|],
      desSum = \ty uc fs -> do
        str <- newName "str"
        obj <- newName "obj"
        [|
          \case
            AE.String $(varP str) -> $(uc (TeVar str))
            AE.Object $(varP obj) -> $(fieldSum ty fs obj)
            _ -> fail $(lift (show ty ++ ": invalid sum object"))
          |]
    }
  where
    desArray ci f = do
      arr <- newName "arr"
      sizeExp <- [|V.length $(varE arr)|]
      [|
        AE.withArray
          $(lift (show ci))
          $(lamE [varP arr] (f (SeExp sizeExp) (VarE arr)))
        |]
    desObject ci f = do
      obj <- newName "obj"
      sizeExp <- [|KM.size $(varE obj)|]
      [|
        AE.withObject
          $(lift (show ci))
          $(lamE [varP obj] (f (SeExp sizeExp) (VarE obj)))
        |]
    fieldSum ty cs o = do
      t <- newName "t"
      v <- newName "v"
      [|
        case KM.toList $(varE o) of
          $(pure (ListP [TupP [VarP t, VarP v]])) -> $(cs (TeVar t) (VarE v))
          _ -> fail $(lift (show ty ++ ": invalid sum object"))
        |]

deserializeClass :: Class
deserializeClass = Class {clsName = ''FromJSON, clsFun = 'parseJSON}

mkProdParseJSON :: ProductOptions -> Name -> ExpQ
mkProdParseJSON = mkDeserializeProd deserializer

mkSumParseJSON :: SumOptions -> Name -> ExpQ
mkSumParseJSON = mkDeserializeSum deserializer

deriveProdFromJSON :: ProductOptions -> Name -> DecsQ
deriveProdFromJSON = deriveDeserializeProd deserializeClass deserializer

deriveSumFromJSON :: SumOptions -> Name -> DecsQ
deriveSumFromJSON = deriveDeserializeSum deserializeClass deserializer

deriveSumJSON :: SumOptions -> Name -> DecsQ
deriveSumJSON =
  deriveSerdeSum
    serializeClass
    serializer
    deserializeClass
    deserializer

deriveProdJSON :: ProductOptions -> Name -> DecsQ
deriveProdJSON =
  deriveSerdeProd
    serializeClass
    serializer
    deserializeClass
    deserializer