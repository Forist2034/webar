{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.Wordpress.RestApi.Model where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector.Primitive as PV
import Webar.Server.Wordpress.Types

data Category = Category
  { catId :: CategoryId,
    catDescription :: HtmlText,
    catLink :: LinkUrl,
    catName :: HtmlText,
    catSlug :: UrlSlug,
    catParent :: CategoryId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Category

newtype RenderedText = RenderedText {tRendered :: HtmlText}
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . tail}
  ''RenderedText

newtype Date = Date UTCTime
  deriving (Show, Eq)

instance FromJSON Date where
  parseJSON =
    Aeson.withText
      "date"
      (\t -> Date <$> parseJSON (Aeson.String (T.snoc t 'Z')))

data Comment = Comment
  { comId :: CommentId,
    comAuthor :: UserId,
    comAuthorName :: Text,
    comAuthorUrl :: Text,
    comContent :: RenderedText,
    comDateGmt :: Date,
    comLink :: LinkUrl,
    comParent :: CommentId,
    comPost :: PostId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Comment

data Media = Media
  { medDateGmt :: Date,
    medId :: MediaId,
    medLink :: LinkUrl,
    medModifiedGmt :: Date,
    medSlug :: UrlSlug,
    medTitle :: RenderedText,
    medAuthor :: UserId,
    medAltText :: HtmlText,
    medCaption :: RenderedText,
    medDescription :: RenderedText,
    medPost :: Maybe PostId,
    medSourceUrl :: LinkUrl
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Media

data Page = Page
  { pageDateGmt :: Date,
    pageId :: PageId,
    pageLink :: LinkUrl,
    pageModifiedGmt :: Date,
    pageSlug :: UrlSlug,
    pageTitle :: RenderedText,
    pageContent :: RenderedText,
    pageAuthor :: UserId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''Page

data PageRevision = PageRevision
  { pagRevAuthor :: UserId,
    pagRevDateGmt :: Date,
    pagRevId :: PageRevisionId,
    pagRevModifiedGmt :: Date,
    pagRevParent :: PageId,
    pagRevSlug :: UrlSlug,
    pagRevTitle :: RenderedText,
    pagRevContent :: RenderedText
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}
  ''PageRevision

data PostFormat
  = PtStandard
  | PtAside
  | PtChat
  | PtGallery
  | PtLink
  | PtImage
  | PtQuote
  | PtStatus
  | PtVideo
  | PtAudio
  deriving (Show, Eq)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''PostFormat

data Post = Post
  { postDateGmt :: Date,
    postId :: PostId,
    postLink :: LinkUrl,
    postModifiedGmt :: Date,
    postSlug :: UrlSlug,
    postTitle :: RenderedText,
    postContent :: RenderedText,
    postAuthor :: UserId,
    postFormat :: PostFormat,
    postCategories :: PV.Vector CategoryId,
    postTags :: PV.Vector TagId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''Post

data PostRevision = PostRevision
  { pRevAuthor :: UserId,
    pRevDateGmt :: Date,
    pRevId :: PostRevisionId,
    pRevModifiedGmt :: Date,
    pRevSlug :: UrlSlug,
    pRevParent :: PostId,
    pRevTitle :: RenderedText,
    pRevContent :: RenderedText
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''PostRevision

data Tag = Tag
  { tagId :: TagId,
    tagDescription :: HtmlText,
    tagLink :: LinkUrl,
    tagName :: HtmlText,
    tagSlug :: UrlSlug
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Tag

data User = User
  { usrId :: UserId,
    usrName :: Text,
    usrUrl :: Text,
    usrDescription :: HtmlText,
    usrLink :: LinkUrl,
    usrSlug :: UrlSlug
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''User