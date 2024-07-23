{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Webar.Server.StackExchange.Api.Object where

import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
import qualified Data.Text as T
import Data.Vector (Vector)
import Data.Word (Word64)
import Webar.Server.StackExchange.Api.ToFilter
import Webar.Server.StackExchange.Types

type Tags = Vector TagName

newtype Date = Date Word64
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToFilter)

newtype Color = Color T.Text
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToFilter)

newtype License = License T.Text
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (ToFilter)

data ExternalLinkType
  = EltWebsite
  | EltTwitter
  | EltGithub
  | EltFacebook
  | EltInstagram
  | EltSupport
  | EltLinkedin
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 3}
  ''ExternalLinkType

data CollectiveExternalLink = CollectiveExternalLink
  { celType :: ExternalLinkType,
    celLink :: LinkUrl
  }
  deriving (Show, Eq)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''CollectiveExternalLink

deriveToFilter "collective_external_link" 3 ''CollectiveExternalLink

data Collective = Collective
  { colDescription :: RawText,
    colExternalLinks :: Vector CollectiveExternalLink,
    colName :: RawText,
    colSlug :: CollectiveSlug,
    colTags :: Tags
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Collective

deriveToFilter "collective" 3 ''Collective

newtype CollectiveRef = CollectiveRef {colRefSlug :: CollectiveSlug}
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}
  ''CollectiveRef

deriveToFilter "collective" 6 ''CollectiveRef

data UserType
  = UtUnregistered
  | UtRegistered
  | UtModerator
  | UtTeamAdmin
  | UtDoesNotExist
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''UserType

data ShallowUser = ShallowUser
  { sUsrAccountId :: Maybe AccountId,
    sUsrUserId :: Maybe UserId,
    sUsrUserType :: UserType
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}
  ''ShallowUser

deriveToFilter "shallow_user" 4 ''ShallowUser

data BadgeType = BtNamed | BtTagBased
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''BadgeType

data Rank = RkGold | RkSilver | RkBronze
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''Rank

data Badge = Badge
  { bAwardCount :: Int,
    bBadgeId :: BadgeId,
    bBadgeType :: BadgeType,
    bDescription :: HtmlContent,
    bLink :: LinkUrl,
    bRank :: Rank
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . tail}
  ''Badge

deriveToFilter "badge" 1 ''Badge

data Answer = Answer
  { ansAnswerId :: AnswerId,
    ansBody :: HtmlContent,
    ansBodyMarkdown :: MarkdownContent,
    ansCanComment :: Maybe Bool,
    ansCanEdit :: Maybe Bool,
    ansCanFlag :: Maybe Bool,
    ansCanSuggestEdit :: Maybe Bool,
    ansCollectives :: Maybe (Vector CollectiveRef),
    ansCommunityOwnedDate :: Maybe Date,
    ansContentLicense :: License,
    ansCreationDate :: Date,
    ansDownVoteCount :: Word,
    ansIsAccepted :: Bool,
    ansLastActivityDate :: Date,
    ansLastEditDate :: Maybe Date,
    ansLastEditor :: Maybe ShallowUser,
    ansLink :: LinkUrl,
    ansLockedDate :: Maybe Date,
    ansOwner :: Maybe ShallowUser,
    ansPostedByCollectives :: Maybe (Vector CollectiveRef),
    ansQuestionId :: QuestionId,
    ansScore :: Int,
    ansTags :: Tags,
    ansTitle :: RawText,
    ansUpVoteCount :: Word
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Answer

deriveToFilter "answer" 3 ''Answer

data PostType
  = PtAnswer
  | PtArticle
  | PtQuestion
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''PostType

data Comment = Comment
  { comBody :: HtmlContent,
    comBodyMarkdown :: MarkdownContent,
    comCanFlag :: Maybe Bool,
    comCommentId :: CommentId,
    comContentLicense :: License,
    comCreationDate :: Date,
    comEdited :: Bool,
    comLink :: LinkUrl,
    comOwner :: Maybe ShallowUser,
    comPostId :: PostId,
    comPostType :: PostType,
    comReplyToUser :: Maybe ShallowUser,
    comScore :: Int
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Comment

deriveToFilter "comment" 3 ''Comment

data OriginalQuestion = OriginalQuestion
  { oqAcceptedAnswerId :: Maybe AnswerId,
    oqQuestionId :: QuestionId,
    oqTitle :: RawText
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''OriginalQuestion

deriveToFilter "original_questions" 2 ''OriginalQuestion

data ClosedDetails = ClosedDetails
  { cdByUsers :: Vector ShallowUser,
    cdDescription :: SafeText,
    cdOriginalQuestions :: Maybe (Vector OriginalQuestion),
    cdReason :: SafeText
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''ClosedDetails

deriveToFilter "closed_details" 2 ''ClosedDetails

data MarkdownExtension
  = MeMathJax
  | MePrettify
  | MeBalsamiq
  | MeJTab
  deriving (Show, Eq, Ord)

deriveFromJSON
  defaultOptions
    { constructorTagModifier = \case
        "MeMathJax" -> "MathJax"
        "MePrettify" -> "Prettify"
        "MeBalsamiq" -> "Balsamiq"
        "MeJTab" -> "jTab"
        a -> a
    }
  ''MarkdownExtension

instance ToFilter MarkdownExtension

data SiteState
  = SsNormal
  | SsClosedBeta
  | SsOpenBeta
  | SsLinkedBeta
  deriving (Show, Eq, Ord)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SiteState

instance ToFilter SiteState

data SiteType
  = StMainSite
  | StBetaSite
  deriving (Show, Eq, Ord)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SiteType

instance ToFilter SiteType

data Styling = Styling
  { styLinkColor :: Color,
    styTagBackgroundColor :: Color,
    styTagForegroundColor :: Color
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Styling

deriveToFilter "styling" 3 ''Styling

data SiteRelation
  = SrParent
  | SrMeta
  | SrChat
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''SiteRelation

data RelatedSite = RelatedSite
  { rsApiSiteParameter :: Maybe ApiSiteParameter,
    rsName :: RawText,
    rsRelation :: SiteRelation,
    rsSiteUrl :: LinkUrl
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''RelatedSite

deriveToFilter "related_site" 2 ''RelatedSite

data Site = Site
  { sitAliases :: Maybe (Vector RawText),
    sitApiSiteParameter :: ApiSiteParameter,
    sitAudience :: RawText,
    sitClosedBetaDate :: Maybe Date,
    sitFaviconUrl :: LinkUrl,
    sitHighResolutionIconUrl :: LinkUrl,
    sitIconUrl :: LinkUrl,
    sitLaunchDate :: Date,
    sitLogoUrl :: LinkUrl,
    sitMarkdownExtensions :: Maybe (Vector MarkdownExtension),
    sitName :: RawText,
    sitOpenBetaDate :: Maybe Date,
    sitRelatedSites :: Maybe (Vector RelatedSite),
    sitSiteState :: SiteState,
    sitSiteType :: SiteType,
    sitSiteUrl :: LinkUrl,
    sitStyling :: Styling
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Site

deriveToFilter "site" 3 ''Site

newtype SiteInfo = SiteInfo {sitInfoSite :: Site}

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}
  ''SiteInfo

deriveToFilter "info" 7 ''SiteInfo

newtype SiteRef = SiteRef {sitRefApiSiteParameter :: ApiSiteParameter} deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}
  ''SiteRef

deriveToFilter "site" 6 ''SiteRef

data MigrationInfo = MigrationInfo
  { migOnDate :: Date,
    migOtherSite :: SiteRef,
    migQuestionId :: QuestionId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''MigrationInfo

deriveToFilter "migration_info" 3 ''MigrationInfo

data Notice = Notice
  { notBody :: SafeText,
    notCreationDate :: Date,
    notOwnerUserId :: UserId
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Notice

deriveToFilter "notice" 3 ''Notice

data Question = Question
  { qAcceptedAnswerId :: Maybe AnswerId,
    qBody :: HtmlContent,
    qBodyMarkdown :: MarkdownContent,
    qCanAnswer :: Maybe Bool,
    qCanClose :: Maybe Bool,
    qCanComment :: Maybe Bool,
    qCanEdit :: Maybe Bool,
    qCanFlag :: Maybe Bool,
    qCanSuggestEdit :: Maybe Bool,
    qCloseVoteCount :: Word,
    qClosedDate :: Maybe Date,
    qClosedDetails :: Maybe ClosedDetails,
    qClosedReason :: Maybe RawText,
    qCollectives :: Maybe (Vector CollectiveRef),
    qCommunityOwnedDate :: Maybe Date,
    qContentLicense :: Maybe License,
    qCreationDate :: Date,
    qDeleteVoteCount :: Word,
    qDownVoteCount :: Word,
    qLastActivityDate :: Date,
    qLastEditDate :: Maybe Date,
    qLastEditor :: Maybe ShallowUser,
    qLink :: LinkUrl,
    qLockedDate :: Maybe Date,
    qMigratedFrom :: Maybe MigrationInfo,
    qMigratedTo :: Maybe MigrationInfo,
    qNotice :: Maybe Notice,
    qOwner :: Maybe ShallowUser,
    qPostedByCollectives :: Maybe (Vector CollectiveRef),
    qProtectedDate :: Maybe Date,
    qQuestionId :: QuestionId,
    qReopenVoteCount :: Word,
    qScore :: Int,
    qTags :: Tags,
    qTitle :: RawText,
    qUpVoteCount :: Word
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . tail}
  ''Question

deriveToFilter "question" 1 ''Question

data RevisionType
  = RtSingleUser
  | RtVoteBased
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''RevisionType

data Revision = Revision
  { revBody :: Maybe HtmlContent,
    revComment :: Maybe HtmlContent,
    revContentLicense :: Maybe License,
    revCreationDate :: Date,
    revIsRollback :: Bool,
    revLastBody :: Maybe HtmlContent,
    revLastTags :: Maybe Tags,
    revLastTitle :: Maybe RawText,
    revPostId :: PostId,
    revPostType :: PostType,
    revRevisionGuid :: RevisionId,
    revRevisionNumber :: Maybe Word,
    revRevisionType :: RevisionType,
    revSetCommunityWiki :: Bool,
    revTags :: Maybe Tags,
    revTitle :: Maybe RawText,
    revUser :: ShallowUser
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Revision

deriveToFilter "revision" 3 ''Revision

data Tag = Tag
  { tagCollectives :: Maybe (Vector CollectiveRef),
    tagName :: TagName,
    tagSynonyms :: Maybe (Vector TagName)
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Tag

deriveToFilter "tag" 3 ''Tag

data TagSynonym = TagSynonym
  { tagSynCreationDate :: Date,
    tagSynFromTag :: TagName,
    tagSynToTag :: TagName
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}
  ''TagSynonym

deriveToFilter "tag_synonym" 6 ''TagSynonym

data TagWiki = TagWiki
  { twBody :: HtmlContent,
    twBodyLastEditDate :: Date,
    twExcerpt :: RawText,
    twExcerptLastEditDate :: Date,
    twLastBodyEditor :: ShallowUser,
    twLastExcerptEditor :: ShallowUser,
    twTagName :: TagName
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''TagWiki

deriveToFilter "tag_wiki" 2 ''TagWiki

data CollectiveRole
  = CrAdmin
  | CrRecognizedMember
  | CrMember
  | CrAnalyst
  | CrLimitedRecognizedMember
  deriving (Show, Eq, Ord)
  deriving anyclass (ToFilter)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''CollectiveRole

data CollectiveMembership = CollectiveMembership
  { cmCollective :: CollectiveRef,
    cmRole :: CollectiveRole
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''CollectiveMembership

deriveToFilter "collective_membership" 2 ''CollectiveMembership

data User = User
  { usrAboutMe :: Maybe HtmlContent,
    usrAccountId :: AccountId,
    usrCollectives :: Maybe (Vector CollectiveMembership),
    usrCreationDate :: Date,
    useDisplayName :: RawText,
    usrIsEmployee :: Bool,
    usrLink :: LinkUrl,
    usrLocation :: Maybe RawText,
    usrProfileImage :: LinkUrl,
    usrReputation :: Int,
    usrUserId :: UserId,
    usrUserType :: UserType,
    usrWebsiteUrl :: Maybe LinkUrl
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''User

deriveToFilter "user" 3 ''User

data FilterType
  = FtSafe
  | FtUnsafe
  | FtInvalid
  deriving (Show, Eq, Ord)

deriveFromJSON
  defaultOptions {constructorTagModifier = camelTo2 '_' . drop 2}
  ''FilterType

data Filter = Filter
  { filFilter :: T.Text,
    filFilterType :: FilterType,
    filIncludedFields :: Vector T.Text
  }
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}
  ''Filter

newtype Wrapper a = Wrapper {wItems :: a}
  deriving (Show)

deriveFromJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . tail}
  ''Wrapper
