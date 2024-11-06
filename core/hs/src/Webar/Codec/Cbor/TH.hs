{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Codec.Cbor.TH
  ( Aeson.camelTo2,
    ProductOptions (fieldLabelModifier),
    defaultProductOptions,
    SumOptions (constructorTagModifier, sumProduct),
    defaultSumOptions,
    deriveProductToCbor,
    deriveSumToCbor,
    deriveProductFromCbor,
    deriveSumFromCbor,
    deriveProductCbor,
    deriveSumCbor,
  )
where

import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Webar.Codec.Cbor.Internal.Decoding
import Webar.Codec.Cbor.Internal.Encoding

newtype ProductOptions = ProductOptions
  { fieldLabelModifier :: String -> String
  }

defaultProductOptions :: ProductOptions
defaultProductOptions =
  ProductOptions {fieldLabelModifier = id}

-- | construstor is not exported, so that new options can be added
data SumOptions = SumOptions
  { constructorTagModifier :: String -> String,
    sumProduct :: ProductOptions
  }

defaultSumOptions :: SumOptions
defaultSumOptions =
  SumOptions
    { constructorTagModifier = id,
      sumProduct = defaultProductOptions
    }

data TypeConInfo = TypeConInfo
  { tciName :: Name,
    tciVars :: [TyVarBndr ()],
    tciRoles :: [Role]
  }

data ProductDec = ProductDec
  { dpTypeCon :: TypeConInfo,
    dpCon :: Con
  }

reifyProductCon :: Name -> Q ([TyVarBndr ()], Con)
reifyProductCon n =
  reify n >>= \case
    TyConI (DataD _ _ tv _ [c] _) -> pure (tv, c)
    TyConI (NewtypeD _ _ tv _ con _) -> pure (tv, con)
    _ -> fail "only data with single constructor or newtype decl is allowed"

reifyProductDec :: Name -> Q ProductDec
reifyProductDec n = do
  (tv, con) <- reifyProductCon n
  roles <- reifyRoles n
  pure
    ProductDec
      { dpTypeCon = TypeConInfo {tciName = n, tciVars = tv, tciRoles = roles},
        dpCon = con
      }

data SumDec = SumDec
  { dsTypeCon :: TypeConInfo,
    dsCons :: [Con]
  }

reifySumCon :: Name -> Q ([TyVarBndr ()], [Con])
reifySumCon n =
  reify n >>= \case
    TyConI (DataD _ _ tv _ cs _) -> pure (tv, cs)
    TyConI (NewtypeD _ _ tv _ con _) -> pure (tv, [con])
    _ -> fail "only data or newtype decl is allowed"

reifySumDec :: Name -> Q SumDec
reifySumDec n = do
  (tv, cons) <- reifySumCon n
  roles <- reifyRoles n
  pure
    SumDec
      { dsTypeCon = TypeConInfo {tciName = n, tciVars = tv, tciRoles = roles},
        dsCons = cons
      }

mkInstance :: Name -> TypeConInfo -> Name -> Exp -> DecQ
mkInstance cls tci funName funBody = do
  (constraints, tyWithArg) <-
    foldlM
      ( \(cs, t) (var, role) -> do
          tyArg <-
            newName
              ( case var of
                  PlainTV (Name (OccName n) _) _ -> n
                  KindedTV (Name (OccName n) _) _ _ -> n
              )
          case role of
            PhantomR -> pure (cs, t `AppT` VarT tyArg)
            _ ->
              pure
                ( ConT cls `AppT` VarT tyArg : cs,
                  t `AppT` VarT tyArg
                )
      )
      ([], ConT (tciName tci))
      (zip (tciVars tci) (tciRoles tci))
  pure
    ( InstanceD
        Nothing
        constraints
        (AppT (ConT cls) tyWithArg)
        [ValD (VarP funName) (NormalB funBody) []]
    )

mkNormalEncoder :: Name -> NEL.NonEmpty BangType -> Q (Pat, Exp)
mkNormalEncoder n bt = do
  (pats, eh :| et) <-
    NEL.unzip
      <$> traverse
        ( \_ -> do
            f <- newName "f"
            pure (VarP f, VarE 'toCbor `AppE` VarE f)
        )
        bt
  pure
    ( ConP n [] (NEL.toList pats),
      foldl' (\e i -> InfixE (Just e) (VarE '(<>)) (Just i)) eh et
    )

sortFields :: (String -> String) -> [VarBangType] -> [(Name, String, Text)]
sortFields modifier fs =
  L.sortOn
    (\(_, _, t) -> t)
    ( fmap
        ( \(n@(Name (OccName ns) _), _, _) ->
            (n, ns, T.pack (modifier ns))
        )
        fs
    )

mkRecordEncoder :: ProductOptions -> Name -> [VarBangType] -> Q (Pat, Exp)
mkRecordEncoder (ProductOptions modifier) con fields = do
  (pats, exprs) <-
    unzip
      <$> traverse
        ( \(n, varName, serNameT) -> do
            var <- newName varName
            expr <- [|encodeField $(lift serNameT) $(varE var)|]
            pure ((n, VarP var), expr)
        )
        (sortFields modifier fields)
  pure
    ( RecP con pats,
      case exprs of
        [] -> VarE 'mempty
        h : t -> foldl' (\e i -> InfixE (Just e) (VarE '(<>)) (Just i)) h t
    )

productEncoder :: ProductOptions -> Con -> ExpQ
productEncoder _ (NormalC _ []) = fail "empty normal constructor is not supported"
productEncoder _ (NormalC con vs@(h : t)) = do
  (pat, expr) <- mkNormalEncoder con (h :| t)
  body <- [|encodeNormalProd $(lift (fromIntegral (length vs) :: Word)) <> $(pure expr)|]
  pure (LamE [pat] body)
productEncoder _ (RecC _ []) = fail "empty record constructor is not supported"
productEncoder opt (RecC con fs) = do
  (pat, expr) <- mkRecordEncoder opt con fs
  body <- [|encodeRecordProd $(lift (fromIntegral (length fs) :: Word)) <> $(pure expr)|]
  pure (LamE [pat] body)
productEncoder _ _ = fail "unsupported constructor"

sumEncoder :: SumOptions -> [Con] -> ExpQ
sumEncoder opt cs =
  LamCaseE
    <$> traverse
      ( \case
          NormalC con [] -> do
            expr <- [|encodeUnitSum $(conNameE con)|]
            pure (Match (ConP con [] []) (NormalB expr) [])
          NormalC con fs@(h : t) -> do
            (pat, expr) <- mkNormalEncoder con (h :| t)
            body <-
              [|encodeNormalSum $(conNameE con) $(conSizeE fs) <> $(pure expr)|]
            pure (Match pat (NormalB body) [])
          RecC con fs -> do
            (pat, expr) <- mkRecordEncoder (sumProduct opt) con fs
            body <-
              [|encodeRecordSum $(conNameE con) $(conSizeE fs) <> $(pure expr)|]
            pure (Match pat (NormalB body) [])
          _ -> fail "unsupported constructor"
      )
      cs
  where
    conNameE :: Name -> ExpQ
    conNameE (Name (OccName name) _) = lift (T.pack (constructorTagModifier opt name))

    conSizeE :: [a] -> ExpQ
    conSizeE s = lift (fromIntegral (length s) :: Word)

mkProductToCborInst :: ProductOptions -> ProductDec -> DecQ
mkProductToCborInst opt dec =
  productEncoder opt (dpCon dec) >>= mkInstance ''ToCbor (dpTypeCon dec) 'toCbor

deriveProductToCbor :: ProductOptions -> Name -> DecsQ
deriveProductToCbor opt name =
  reifyProductDec name >>= fmap L.singleton . mkProductToCborInst opt

mkSumToCborInst :: SumOptions -> SumDec -> DecQ
mkSumToCborInst opt dec =
  sumEncoder opt (dsCons dec) >>= mkInstance ''ToCbor (dsTypeCon dec) 'toCbor

deriveSumToCbor :: SumOptions -> Name -> DecsQ
deriveSumToCbor opt name =
  reifySumDec name >>= fmap L.singleton . mkSumToCborInst opt

mkNormalDecoder :: Name -> NonEmpty BangType -> ExpQ
mkNormalDecoder n (_ :| t) = do
  expr <- [|$(conE n) <$> fromCbor|]
  foldM (\e _ -> [|$(pure e) <*> fromCbor|]) expr t

mkRecordDecoder :: ProductOptions -> Name -> [VarBangType] -> ExpQ
mkRecordDecoder opt n fs = do
  (stmts, fields) <-
    unzip
      <$> traverse
        ( \(field, varName, serName) -> do
            var <- newName varName
            expr <- [|decodeFieldOf $(lift serName)|]
            pure (BindS (VarP var) expr, (field, VarE var))
        )
        (sortFields (fieldLabelModifier opt) fs)
  pure (DoE Nothing (stmts ++ [NoBindS (VarE 'pure `AppE` RecConE n fields)]))

productDecoder :: ProductOptions -> Con -> ExpQ
productDecoder _ (NormalC _ []) = fail "empty normal constructor is not supported"
productDecoder _ (NormalC n fs@(h : t)) =
  [|decodeNormalProdOf $(lift (length fs)) >> $(mkNormalDecoder n (h :| t))|]
productDecoder opt (RecC n fs) =
  [|decodeRecordProdOf $(lift (length fs)) >> $(mkRecordDecoder opt n fs)|]
productDecoder _ _ = fail "unsupported constructor"

sumDecoder :: SumOptions -> [Con] -> ExpQ
sumDecoder opt cs = do
  matches <-
    traverse
      ( \case
          NormalC n [] ->
            Match
              <$> [p|SkUnit $(conNameP n)|]
              <*> fmap NormalB [|pure $(conE n)|]
              <*> pure []
          NormalC n fs@(h : t) ->
            Match
              <$> [p|SkNormal $(conNameP n) $(conSizeP fs)|]
              <*> fmap NormalB (mkNormalDecoder n (h :| t))
              <*> pure []
          RecC n fs ->
            Match
              <$> [p|SkRecord $(conNameP n) $(conSizeP fs)|]
              <*> fmap NormalB (mkRecordDecoder (sumProduct opt) n fs)
              <*> pure []
          _ -> fail "unsupported constructor"
      )
      cs
  failMatch <-
    Match WildP
      <$> fmap NormalB [|fail "invalid sum data"|]
      <*> pure []
  [|decodeSum >>= $(pure (LamCaseE (matches ++ [failMatch])))|]
  where
    conNameP :: Name -> PatQ
    conNameP (Name (OccName name) _) =
      pure
        ( LitP (StringL (constructorTagModifier opt name))
        )

    conSizeP :: [a] -> PatQ
    conSizeP l = pure (LitP (IntegerL (fromIntegral (length l))))

mkProductFromCborInst :: ProductOptions -> ProductDec -> DecQ
mkProductFromCborInst opt dec =
  productDecoder opt (dpCon dec)
    >>= mkInstance ''FromCbor (dpTypeCon dec) 'fromCbor

deriveProductFromCbor :: ProductOptions -> Name -> DecsQ
deriveProductFromCbor opt n =
  reifyProductDec n >>= fmap L.singleton . mkProductFromCborInst opt

mkSumFromCborInst :: SumOptions -> SumDec -> DecQ
mkSumFromCborInst opt dec =
  sumDecoder opt (dsCons dec)
    >>= mkInstance ''FromCbor (dsTypeCon dec) 'fromCbor

deriveSumFromCbor :: SumOptions -> Name -> DecsQ
deriveSumFromCbor opt n =
  reifySumDec n >>= fmap L.singleton . mkSumFromCborInst opt

deriveProductCbor :: ProductOptions -> Name -> DecsQ
deriveProductCbor opt n =
  reifyProductDec n >>= \dec ->
    liftA2
      (\t f -> [t, f])
      (mkProductToCborInst opt dec)
      (mkProductFromCborInst opt dec)

deriveSumCbor :: SumOptions -> Name -> DecsQ
deriveSumCbor opt n =
  reifySumDec n >>= \dec ->
    liftA2
      (\t f -> [t, f])
      (mkSumToCborInst opt dec)
      (mkSumFromCborInst opt dec)