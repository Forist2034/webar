{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.Types where

import Data.Text (Text)
import Data.Vector.Primitive (Prim)
import Data.Word (Word64)
import Webar.Data.Cbor (FromCbor, ToCbor)
import Webar.Data.Json (FromJSON, ToJSON)
import Webar.Data.TH
import Webar.Types (Host)

newtype CategoryId = CategoryId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype CommentId = CommentId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype MediaId = MediaId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype PageId = PageId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype PageRevisionId = PageRevisionId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype PostId = PostId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype PostRevisionId = PostRevisionId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype TagId = TagId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype UserId = UserId Word64
  deriving (Show, Eq, Ord, Prim, FromCbor, ToCbor, FromJSON, ToJSON)

newtype HtmlText = HtmlText Text
  deriving (Show, FromJSON)

newtype LinkUrl = LinkUrl Text
  deriving (Show, FromJSON)

newtype UrlSlug = UrlSlug Text
  deriving (Show, FromJSON)

data Address = Address Host Text
  deriving (Show, Eq)

deriveProdData defaultProductOptions ''Address

type Instance = Address
