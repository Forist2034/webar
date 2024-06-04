{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Data.Cbor.TH
  ( ProductOptions (..),
    SumOptions (..),
    camelTo2,
    mkSumToCbor,
    mkProdToCbor,
    deriveSumToCbor,
    deriveProdToCbor,
    mkSumFromCbor,
    mkProdFromCbor,
    deriveSumFromCbor,
    deriveProdFromCbor,
    deriveSumCbor,
    deriveProdCbor,
  )
where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Control.Applicative (Applicative (liftA2))
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Webar.Data.Cbor
import Webar.Data.Internal.TH

serializer :: Serializer
serializer =
  Serializer
    { serNamedField = \k v -> [|encodeString $(lift k) <> toCbor $(pure v)|],
      serField = \e -> [|toCbor $(pure e)|],
      serUnitSum = \c -> [|encodeString $(lift c)|],
      serUnarySum = \c v ->
        [|encodeMapLen 1 <> (encodeString $(lift c) <> toCbor $(pure v))|],
      serNormalSum = \c fs ->
        [|encodeMapLen 1 <> (encodeString $(lift c) <> $(listExpr fs))|],
      serRecSum = \c fs ->
        [|encodeMapLen 1 <> (encodeString $(lift c) <> $(mapExpr fs))|],
      serRecProd = mapExpr,
      serNormalProd = listExpr
    }
  where
    membersExpr (h :| t) =
      foldl'
        (\e f -> InfixE (Just e) (VarE '(<>)) (Just f))
        h
        t
    listExpr fs =
      [|
        encodeListLen $(lift (fromIntegral (length fs) :: Word))
          <> $(pure (membersExpr fs))
        |]
    mapExpr fs =
      [|
        encodeMapLen $(lift (fromIntegral (length fs) :: Word))
          <> $(pure (membersExpr fs))
        |]

mkSumToCbor :: SumOptions -> Name -> ExpQ
mkSumToCbor opt n =
  reify n >>= \case
    TyConI dec -> mkSerializeSum serializer opt dec
    _ -> error "Type decl expected"

mkProdToCbor :: ProductOptions -> Name -> ExpQ
mkProdToCbor opt n =
  reify n >>= \case
    TyConI dec -> mkSerializeProd serializer opt dec
    _ -> error "Type decl expected"

deriveSumToCbor :: SumOptions -> Name -> DecsQ
deriveSumToCbor opt n =
  [d|
    instance ToCbor $(conT n) where
      toCbor = $(mkSumToCbor opt n)
    |]

deriveProdToCbor :: ProductOptions -> Name -> DecsQ
deriveProdToCbor opt n =
  [d|
    instance ToCbor $(conT n) where
      toCbor = $(mkProdToCbor opt n)
    |]

decodeNamedField :: (FromCbor a) => Text -> Decoder s a
decodeNamedField fn =
  decodeStringCanonical >>= \t ->
    if t == fn
      then fromCbor
      else fail ("unexpected field: " ++ show t ++ " expect " ++ show fn)
{-# INLINEABLE decodeNamedField #-}

deserializer :: Deserializer () () ()
deserializer =
  Deserializer
    { desField = \_ _ -> [|fromCbor|],
      desNamedField = \_ name -> [|decodeNamedField $(lift name)|],
      desNormalProd = const desList,
      desRecProd = const desMap,
      desUnaryCon = \_ ci -> [|$(conE (ciCon ci)) <$> fromCbor|],
      desNormalCon = \_ _ -> desList,
      desRecCon = \_ _ -> desMap,
      desUnitSum = const desUnitS,
      desFieldSum = desFieldS,
      desSum = \ty uc fc ->
        [|
          peekTokenType >>= \case
            TypeString -> $(desUnitS uc)
            TypeMapLen -> $(desFieldS ty fc)
            _ -> fail $(lift (show ty ++ ": Invalid type, expect string or map"))
          |]
    }
  where
    desList fields = [|decodeListLenCanonical >>= $(fields SeArg ())|]
    desMap fields = [|decodeMapLenCanonical >>= $(fields SeArg ())|]
    desUnitS cs = [|decodeStringCanonical >>= $(cs TeArg)|]
    desFieldS ty cs =
      [|
        decodeMapLenCanonical >>= \case
          1 -> decodeStringCanonical >>= $(cs TeArg ())
          _ -> fail $(lift (show ty ++ ": invalid sum map"))
        |]

mkSumFromCbor :: SumOptions -> Name -> ExpQ
mkSumFromCbor opt n =
  reify n >>= \case
    TyConI dec -> mkDeserializeSum deserializer opt dec
    _ -> error "data or newtype expected"

mkProdFromCbor :: ProductOptions -> Name -> ExpQ
mkProdFromCbor opt n =
  reify n >>= \case
    TyConI dec -> mkDeserializeProd deserializer opt dec
    _ -> error "data with single constructor or newtype expected"

deriveSumFromCbor :: SumOptions -> Name -> DecsQ
deriveSumFromCbor opt n =
  [d|
    instance FromCbor $(conT n) where
      fromCbor = $(mkSumFromCbor opt n)
    |]

deriveProdFromCbor :: ProductOptions -> Name -> DecsQ
deriveProdFromCbor opt n =
  [d|
    instance FromCbor $(conT n) where
      fromCbor = $(mkProdFromCbor opt n)
    |]

deriveSumCbor :: SumOptions -> Name -> DecsQ
deriveSumCbor opt n =
  liftA2 (++) (deriveSumFromCbor opt n) (deriveSumToCbor opt n)

deriveProdCbor :: ProductOptions -> Name -> DecsQ
deriveProdCbor opt n =
  liftA2 (++) (deriveProdFromCbor opt n) (deriveProdToCbor opt n)
