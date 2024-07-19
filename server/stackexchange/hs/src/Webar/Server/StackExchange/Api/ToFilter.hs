{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Webar.Server.StackExchange.Api.ToFilter where

import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Webar.Data.Json (ToJSON)
import Webar.Data.Json.TH (camelTo2)
import Webar.Server.StackExchange.Types

newtype Fields = Fields (Map Text (Vector Text))
  deriving (Show, ToJSON)

newtype Param = Param (Map Text Text)
  deriving (Show, ToJSON)

class ToFilter a where
  addToFields :: Proxy a -> Fields -> Fields
  addToFields _ f = f
  addToParam :: Proxy a -> Param -> Param
  addToParam _ p = p

instance ToFilter Word

instance ToFilter Word64

instance ToFilter Int

instance ToFilter Bool

instance ToFilter AnswerId

instance ToFilter BadgeId

instance ToFilter CollectiveSlug

instance ToFilter CommentId

instance ToFilter QuestionId

instance ToFilter PostId

instance ToFilter RevisionId

instance ToFilter TagName

instance ToFilter UserId

instance ToFilter AccountId

instance ToFilter ApiSiteParameter

instance ToFilter LinkUrl

instance ToFilter HtmlContent

instance ToFilter MarkdownContent

instance ToFilter RawText

instance ToFilter SafeText

instance (ToFilter t) => ToFilter (Maybe t) where
  addToFields _ = addToFields @t Proxy
  addToParam _ = addToParam @t Proxy

instance (ToFilter t) => ToFilter (Vector t) where
  addToFields _ = addToFields @t Proxy
  addToParam _ = addToParam @t Proxy

deriveToFilter :: String -> Int -> Name -> DecsQ
deriveToFilter tyName modifier n =
  let tName = T.pack tyName
      tNameQ = lift tName
   in do
        fields <-
          reify n >>= \case
            TyConI (DataD _ _ [] _ [RecC _ fs] _) -> pure fs
            TyConI (NewtypeD _ _ [] _ (RecC _ fs) _) -> pure fs
            _ -> fail "unexpected type. expect record"
        let parentF fun =
              newName "f" <&> \f ->
                LamE
                  [VarP f]
                  ( foldl'
                      ( \e (_, _, t) ->
                          VarE fun
                            `AppE` SigE (ConE 'Proxy) (ConT ''Proxy `AppT` t)
                            `AppE` e
                      )
                      (VarE f)
                      fields
                  )
            fieldNames =
              map
                (\(Name (OccName f) _, _, _) -> T.pack (camelTo2 '_' (drop modifier f)))
                fields
            fieldParam =
              T.intercalate
                ";"
                (fmap (\f -> tName <> "." <> f) fieldNames)
        [d|
          instance ToFilter $(pure (ConT n)) where
            addToFields _ f@(Fields mp)
              | M.member $tNameQ mp = f
              | otherwise =
                  $(parentF 'addToFields)
                    ( Fields
                        ( M.insert
                            $tNameQ
                            (V.fromList $(lift fieldNames))
                            mp
                        )
                    )
            addToParam _ f@(Param mp)
              | M.member $tNameQ mp = f
              | otherwise =
                  $(parentF 'addToParam)
                    (Param (M.insert $tNameQ $(lift fieldParam) mp))
          |]
