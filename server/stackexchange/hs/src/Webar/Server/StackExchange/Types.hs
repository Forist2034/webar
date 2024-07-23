{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Webar.Server.StackExchange.Types
  ( AnswerId (..),
    BadgeId (..),
    CollectiveSlug (..),
    CommentId (..),
    QuestionId (..),
    PostId (..),
    TagName (..),
    UserId (..),
    RevisionId (..),
    AccountId (..),
    FilterId (..),
    ApiSiteParameter (apiSiteParamToText),
    LinkUrl (..),
    HtmlContent (..),
    MarkdownContent (..),
    RawText (..),
    SafeText (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Word (Word64)
import Webar.Data.Cbor
import Webar.Data.Json
import Webar.Digest (Digest)

newtype AnswerId = AnswerId Word64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype BadgeId = BadgeId Word64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype CollectiveSlug = CollectiveSlug Text
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype CommentId = CommentId Word64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype QuestionId = QuestionId Word64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype PostId = PostId Word64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype RevisionId = RevisionId UUID
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype TagName = TagName Text
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype UserId = UserId Int64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype AccountId = AccountId Int64
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype FilterId = FilterId Digest
  deriving (Show, Eq, Ord, FromJSON, ToJSON, FromCbor, ToCbor)

newtype ApiSiteParameter = ApiSiteParameter {apiSiteParamToText :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      FromCbor,
      ToCbor
    )

newtype LinkUrl = LinkUrl Text
  deriving (Show, Eq, FromJSON, ToJSON, FromCbor, ToCbor)

newtype HtmlContent = HtmlContent Text
  deriving (Show, Eq, FromJSON, ToJSON, FromCbor, ToCbor)

-- | raw markdown content, need sanitizing before use
newtype MarkdownContent = MarkdownContent Text
  deriving (Show, Eq, FromJSON, ToJSON, FromCbor, ToCbor)

-- | raw user input, need sanitizing before use
newtype RawText = RawText Text
  deriving (Show, Eq, FromJSON, ToJSON, FromCbor, ToCbor)

-- | sanitized user input
newtype SafeText = SafeText Text
  deriving (Show, Eq, FromJSON, ToJSON, FromCbor, ToCbor)
