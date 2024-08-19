{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Data.Internal.TH
  ( ProductOptions (fieldLabelModifier),
    defaultProductOptions,
    SumOptions (constructorTagModifier, sumProduct),
    defaultSumOptions,
    camelTo2,
    Class (..),
    Serializer (..),
    mkSerializeProd,
    deriveSerializeProd,
    mkSerializeSum,
    deriveSerializeSum,
    ConInfo (ciType, ciCon),
    TagExp (..),
    SizeExp (..),
    Deserializer (..),
    mkDeserializeProd,
    deriveDeserializeProd,
    mkDeserializeSum,
    deriveDeserializeSum,
    deriveSerdeProd,
    deriveSerdeSum,
  )
where

import Data.Aeson (camelTo2)
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | constructor is not exported, so that new options can be added
newtype ProductOptions = ProductOptions
  { fieldLabelModifier :: String -> String
  }

defaultProductOptions :: ProductOptions
defaultProductOptions =
  ProductOptions
    { fieldLabelModifier = id
    }

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

data Class = Class
  { clsName :: Name,
    clsFun :: Name
  }

data ProductDec = ProductDec
  { dpRoles :: [Role],
    dpTyVars :: [TyVarBndr ()],
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
  pure ProductDec {dpRoles = roles, dpTyVars = tv, dpCon = con}

data SumDec = SumDec
  { dsRoles :: [Role],
    dsTyVars :: [TyVarBndr ()],
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
  pure SumDec {dsRoles = roles, dsTyVars = tv, dsCons = cons}

mkInstance :: Class -> Name -> [TyVarBndr ()] -> [Role] -> Exp -> DecsQ
mkInstance cls ty tv rs e = do
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
            PhantomR -> pure (cs, AppT t (VarT tyArg))
            _ ->
              pure
                ( AppT (ConT (clsName cls)) (VarT tyArg) : cs,
                  AppT t (VarT tyArg)
                )
      )
      ([], ConT ty)
      (zip tv rs)
  pure
    [ InstanceD
        Nothing
        constraints
        (AppT (ConT (clsName cls)) tyWithArg)
        [ValD (VarP (clsFun cls)) (NormalB e) []]
    ]

data Serializer = Serializer
  { serField :: Exp -> ExpQ,
    serNamedField :: Text -> Exp -> ExpQ,
    -- | single normal constructor
    serNormalProd :: NonEmpty Exp -> ExpQ,
    -- | single record constructor
    serRecProd :: NonEmpty Exp -> ExpQ,
    serUnitSum :: Text -> ExpQ,
    serUnarySum :: Text -> Exp -> ExpQ,
    serNormalSum :: Text -> NonEmpty Exp -> ExpQ,
    serRecSum :: Text -> NonEmpty Exp -> ExpQ
  }

-- constructor with no field is not supported because not sure what to do

mkNormalEncoder :: Serializer -> Name -> NonEmpty BangType -> Q (Pat, NonEmpty Exp)
mkNormalEncoder ser cn cs = do
  (pats, exprs) <-
    NEL.unzip
      <$> traverse
        ( \_ -> do
            fn <- newName "f"
            expr <- serField ser (VarE fn)
            pure (VarP fn, expr)
        )
        cs
  pure (ConP cn [] (NEL.toList pats), exprs)

sortRecFields :: ProductOptions -> [VarBangType] -> [(Text, Name)]
sortRecFields opt fs =
  L.sortOn
    fst
    ( fmap
        ( \(n@(Name (OccName ns) _), _, _) ->
            (T.pack (fieldLabelModifier opt ns), n)
        )
        fs
    )

mkRecordEncoder :: Serializer -> ProductOptions -> Name -> [VarBangType] -> Q (Pat, NonEmpty Exp)
mkRecordEncoder ser opt cn fs = do
  (pats, exprs) <-
    unzip
      <$> traverse
        ( \(serName, fieldName) -> do
            field <- newName "f"
            expr <- serNamedField ser serName (VarE field)
            pure ((fieldName, VarP field), expr)
        )
        (sortRecFields opt fs)
  pure (RecP cn pats, NEL.fromList exprs)

productToSerialize :: Serializer -> ProductOptions -> Con -> ExpQ
productToSerialize _ _ (NormalC _ []) = error "empty constructor is not supported"
productToSerialize ser _ (NormalC cn (h : t)) = do
  (pat, exprs) <- mkNormalEncoder ser cn (h :| t)
  body <- serNormalProd ser exprs
  pure (LamE [pat] body)
productToSerialize _ _ (RecC _ []) = error "empty record is not supported"
productToSerialize ser opt (RecC cn fs) = do
  (pat, exprs) <- mkRecordEncoder ser opt cn fs
  body <- serRecProd ser exprs
  pure (LamE [pat] body)
productToSerialize _ _ _ = error "Unsupported constructor"

serializedSum :: SumOptions -> Name -> String
serializedSum opt (Name (OccName cns) _) = constructorTagModifier opt cns

serializedSumTxt :: SumOptions -> Name -> Text
serializedSumTxt opt n = T.pack (serializedSum opt n)

sumToSerialize :: Serializer -> SumOptions -> [Con] -> ExpQ
sumToSerialize ser opt cs = do
  v <- newName "v"
  matches <-
    traverse
      ( \case
          NormalC cn [] -> do
            body <- serUnitSum ser (serializedSumTxt opt cn)
            pure (Match (ConP cn [] []) (NormalB body) [])
          NormalC cn [_] -> do
            fn <- newName "f"
            body <- serUnarySum ser (serializedSumTxt opt cn) (VarE fn)
            pure (Match (ConP cn [] [VarP fn]) (NormalB body) [])
          NormalC cn (h : t) -> do
            (pat, exprs) <- mkNormalEncoder ser cn (h :| t)
            body <- serNormalSum ser (serializedSumTxt opt cn) exprs
            pure (Match pat (NormalB body) [])
          RecC _ [] -> error "empty record is not supported"
          RecC cn fs -> do
            (pat, exprs) <- mkRecordEncoder ser (sumProduct opt) cn fs
            body <- serRecSum ser (serializedSumTxt opt cn) exprs
            pure (Match pat (NormalB body) [])
          _ -> error "Unsupported constructor"
      )
      cs
  pure (LamE [VarP v] (CaseE (VarE v) matches))

mkSerializeProd :: Serializer -> ProductOptions -> Name -> ExpQ
mkSerializeProd ser opt n = reifyProductCon n >>= productToSerialize ser opt . snd

mkSerializeProdInst :: Class -> Serializer -> ProductOptions -> Name -> ProductDec -> DecsQ
mkSerializeProdInst cls ser opt n d =
  productToSerialize ser opt (dpCon d) >>= mkInstance cls n (dpTyVars d) (dpRoles d)

deriveSerializeProd :: Class -> Serializer -> ProductOptions -> Name -> DecsQ
deriveSerializeProd cls ser opt n =
  reifyProductDec n >>= mkSerializeProdInst cls ser opt n

mkSerializeSum :: Serializer -> SumOptions -> Name -> ExpQ
mkSerializeSum ser opt n = reifySumCon n >>= sumToSerialize ser opt . snd

mkSerializeSumInst :: Class -> Serializer -> SumOptions -> Name -> SumDec -> DecsQ
mkSerializeSumInst cls ser opt n d =
  sumToSerialize ser opt (dsCons d) >>= mkInstance cls n (dsTyVars d) (dsRoles d)

deriveSerializeSum :: Class -> Serializer -> SumOptions -> Name -> DecsQ
deriveSerializeSum cls ser opt n =
  reifySumDec n >>= mkSerializeSumInst cls ser opt n

data ConInfo = ConInfo
  { ciType :: !Name,
    ciCon :: !Name
  }

data TagExp = TeArg | TeVar Name

data SizeExp = SeArg | SeExp Exp

data Deserializer normalObj recObj sum = Deserializer
  { desField :: normalObj -> Int -> ExpQ,
    desNamedField :: recObj -> Text -> ExpQ,
    desNormalProd :: ConInfo -> (SizeExp -> normalObj -> ExpQ) -> ExpQ,
    desRecProd :: ConInfo -> (SizeExp -> recObj -> ExpQ) -> ExpQ,
    desUnaryCon :: sum -> ConInfo -> ExpQ,
    desNormalCon :: sum -> ConInfo -> (SizeExp -> normalObj -> ExpQ) -> ExpQ,
    desRecCon :: sum -> ConInfo -> (SizeExp -> recObj -> ExpQ) -> ExpQ,
    desUnitSum :: Name -> (TagExp -> ExpQ) -> ExpQ,
    desFieldSum :: Name -> (TagExp -> sum -> ExpQ) -> ExpQ,
    desSum :: Name -> (TagExp -> ExpQ) -> (TagExp -> sum -> ExpQ) -> ExpQ
  }

mkNormalDecoder ::
  Deserializer no ro s ->
  Name ->
  NonEmpty BangType ->
  SizeExp ->
  no ->
  ExpQ
mkNormalDecoder des cn fs@(_ :| t) se no = do
  body <-
    foldl'
      (\e (idx, _) -> [|$e <*> $(desField des no idx)|])
      [|$(conE cn) <$> $(desField des no 0)|]
      (zip [1 ..] t)
  let len = length fs
      errMsg = show cn ++ ": length mismatch, expected " ++ show len ++ ", got "
  case se of
    SeArg ->
      [|
        \case
          $(pure (LitP (IntegerL (fromIntegral len)))) -> $(pure body)
          l -> fail ($(lift errMsg) ++ show l)
        |]
    SeExp e ->
      [|
        if $(pure e) == $(lift len)
          then $(pure body)
          else fail ($(lift errMsg) ++ show $(pure e))
        |]

mkRecordDecoder ::
  Deserializer no ro s ->
  ProductOptions ->
  Name ->
  [VarBangType] ->
  SizeExp ->
  ro ->
  ExpQ
mkRecordDecoder des opt cn fs se ro = do
  (fields, stmts) <-
    unzip
      <$> traverse
        ( \(serName, field) -> do
            f <- newName "f"
            expr <- desNamedField des ro serName
            pure ((field, VarE f), BindS (VarP f) expr)
        )
        (sortRecFields opt fs)
  let body = DoE Nothing (stmts ++ [NoBindS (VarE 'pure `AppE` RecConE cn fields)])
      len = length fs
      errMsg = show cn ++ ": size mismatch, expected " ++ show len ++ ", got "
  case se of
    SeArg ->
      [|
        \case
          $(pure (LitP (IntegerL (fromIntegral len)))) -> $(pure body)
          l -> fail ($(lift errMsg) ++ show l)
        |]
    SeExp e ->
      [|
        if $(pure e) == $(lift len)
          then $(pure body)
          else fail ($(lift errMsg) ++ show $(pure e))
        |]

deserializeToProd :: Deserializer no ro s -> ProductOptions -> Name -> Con -> ExpQ
deserializeToProd _ _ _ (NormalC _ []) = error "empty constructor is not supported"
deserializeToProd des _ ty (NormalC cn (h : t)) =
  desNormalProd
    des
    ConInfo {ciType = ty, ciCon = cn}
    (mkNormalDecoder des cn (h :| t))
deserializeToProd _ _ _ (RecC _ []) = error "empty record is not supported"
deserializeToProd des opt ty (RecC cn fs) =
  desRecProd
    des
    ConInfo {ciType = ty, ciCon = cn}
    (mkRecordDecoder des opt cn fs)
deserializeToProd _ _ _ _ = error "Unsupported constructor"

mkDeserializeProd :: Deserializer no ro s -> ProductOptions -> Name -> ExpQ
mkDeserializeProd des opt n = reifyProductCon n >>= deserializeToProd des opt n . snd

mkDeserializeProdInst :: Class -> Deserializer no ro s -> ProductOptions -> Name -> ProductDec -> DecsQ
mkDeserializeProdInst cls des opt n d =
  deserializeToProd des opt n (dpCon d) >>= mkInstance cls n (dpTyVars d) (dpRoles d)

deriveDeserializeProd :: Class -> Deserializer no ro s -> ProductOptions -> Name -> DecsQ
deriveDeserializeProd cls des opt n =
  reifyProductDec n >>= mkDeserializeProdInst cls des opt n

groupCon ::
  [Name] ->
  [(Name, Either (NonEmpty BangType) [VarBangType])] ->
  [Con] ->
  ([Name], [(Name, Either (NonEmpty BangType) [VarBangType])])
groupCon unitAcc fieldAcc [] = (unitAcc, fieldAcc)
groupCon unitAcc fieldAcc (h : t) = case h of
  NormalC cn [] -> groupCon (cn : unitAcc) fieldAcc t
  NormalC cn (fh : ft) ->
    groupCon
      unitAcc
      ((cn, Left (fh :| ft)) : fieldAcc)
      t
  RecC _ [] -> error "empty record is not supported"
  RecC cn fs ->
    groupCon
      unitAcc
      ((cn, Right fs) : fieldAcc)
      t
  _ -> error "Unsupported constructor"

deserializeToSum :: Deserializer no ro s -> SumOptions -> Name -> [Con] -> ExpQ
deserializeToSum des opt ty cs = do
  invalidTag <- do
    t <- newName "t"
    expr <- [|fail ($(lift (show ty ++ ": unknown constructor tag ")) ++ show $(varE t))|]
    pure (Match (VarP t) (NormalB expr) [])
  let unitBody =
        fmap
          ( \cn ->
              Match
                (LitP (StringL (serializedSum opt cn)))
                (NormalB (VarE 'pure `AppE` ConE cn))
                []
          )
          unitCon
          ++ [invalidTag]
  let unitDecoder te =
        pure
          ( case te of
              TeArg -> LamCaseE unitBody
              TeVar v -> CaseE (VarE v) unitBody
          )
  case (unitCon, fieldCon) of
    ([], []) -> error "empty data is not supported"
    (_ : _, []) -> desUnitSum des ty unitDecoder
    ([], _ : _) -> desFieldSum des ty (fieldDecoder invalidTag)
    (_ : _, _ : _) -> desSum des ty unitDecoder (fieldDecoder invalidTag)
  where
    (unitCon, fieldCon) = groupCon [] [] cs

    fieldDecoder invalidTag te s = do
      matches <-
        traverse
          ( \(cn, fields) ->
              let info = ConInfo {ciType = ty, ciCon = cn}
               in fmap
                    ( \expr ->
                        Match
                          (LitP (StringL (serializedSum opt cn)))
                          (NormalB expr)
                          []
                    )
                    ( case fields of
                        Left (_ :| []) -> desUnaryCon des s info
                        Left fs -> desNormalCon des s info (mkNormalDecoder des cn fs)
                        Right fs ->
                          desRecCon
                            des
                            s
                            info
                            (mkRecordDecoder des (sumProduct opt) cn fs)
                    )
          )
          fieldCon
      let body = matches ++ [invalidTag]
      pure
        ( case te of
            TeArg -> LamCaseE body
            TeVar v -> CaseE (VarE v) body
        )

mkDeserializeSum :: Deserializer no ro s -> SumOptions -> Name -> ExpQ
mkDeserializeSum des opt n = reifySumCon n >>= deserializeToSum des opt n . snd

mkDeserializeSumInst :: Class -> Deserializer no ro s -> SumOptions -> Name -> SumDec -> DecsQ
mkDeserializeSumInst cls des opt n d =
  deserializeToSum des opt n (dsCons d) >>= mkInstance cls n (dsTyVars d) (dsRoles d)

deriveDeserializeSum :: Class -> Deserializer no ro s -> SumOptions -> Name -> DecsQ
deriveDeserializeSum cls des opt n =
  reifySumDec n >>= mkDeserializeSumInst cls des opt n

deriveSerdeProd ::
  Class ->
  Serializer ->
  Class ->
  Deserializer no ro s ->
  ProductOptions ->
  Name ->
  DecsQ
deriveSerdeProd serCls ser desCls des opt n =
  reifyProductDec n >>= \d ->
    liftA2
      (++)
      (mkSerializeProdInst serCls ser opt n d)
      (mkDeserializeProdInst desCls des opt n d)

deriveSerdeSum ::
  Class ->
  Serializer ->
  Class ->
  Deserializer no ro s ->
  SumOptions ->
  Name ->
  DecsQ
deriveSerdeSum serCls ser desCls des opt n =
  reifySumDec n >>= \d ->
    liftA2
      (++)
      (mkSerializeSumInst serCls ser opt n d)
      (mkDeserializeSumInst desCls des opt n d)