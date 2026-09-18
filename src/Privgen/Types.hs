{-# LANGUAGE OverloadedStrings #-}

-- | The shared vocabulary every other module speaks.
--
-- Each enumeration carries an exhaustive @*Label@ function. Because those are
-- exhaustive @case@s compiled with @-Wall@, adding a constructor breaks the
-- build until it has been given a YAML name — which is the point. A new legal
-- dimension should not be able to enter the system silently.
module Privgen.Types
  ( -- * Slugs
    Slug
  , unSlug
  , mkSlug
    -- * Enumerations
  , Region (..)
  , Platform (..)
  , Audience (..)
  , DataCategory (..)
  , Purpose (..)
  , Feature (..)
  , Monetization (..)
  , RefundRouting (..)
    -- * Labels
  , regionLabel
  , regionName
  , platformLabel
  , platformName
  , audienceLabel
  , dataCategoryLabel
  , dataCategoryName
  , purposeLabel
  , purposeName
  , featureLabel
  , monetizationLabel
  , refundRoutingLabel
    -- * Decoding helpers
  , enumParser
  , labelTable
  ) where

import           Data.Aeson       (FromJSON (..), Value, withText)
import           Data.Aeson.Types (Parser)
import           Data.List        (intercalate)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Web.HttpApiData  (FromHttpApiData (..))

-- ---------------------------------------------------------------------------
-- Slugs
-- ---------------------------------------------------------------------------

-- | A URL path segment identifying one game.
--
-- Validation here is a security control, not hygiene: the previous
-- implementation concatenated an unvalidated capture straight into a file
-- path. Nothing downstream can construct a 'Slug' without going through
-- 'mkSlug'.
newtype Slug = Slug Text
  deriving (Eq, Ord, Show)

unSlug :: Slug -> Text
unSlug (Slug t) = t

-- | Accepts @^[a-z0-9][a-z0-9-]{0,63}$@.
mkSlug :: Text -> Either Text Slug
mkSlug t
  | T.null t = Left "slug is empty"
  | T.length t > 64 =
      Left ("slug is longer than 64 characters: " <> t)
  | not (isSlugStart (T.head t)) =
      Left ("slug must start with a lowercase letter or digit: " <> t)
  | not (T.all isSlugChar t) =
      Left ("slug may only contain lowercase letters, digits and hyphens: " <> t)
  | otherwise = Right (Slug t)

isSlugStart :: Char -> Bool
isSlugStart c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

isSlugChar :: Char -> Bool
isSlugChar c = isSlugStart c || c == '-'

-- | Servant uses this for @Capture "slug" Slug@, so a malformed slug is a 400
-- decided before any handler runs.
instance FromHttpApiData Slug where
  parseUrlPiece = mkSlug

instance FromJSON Slug where
  parseJSON = withText "Slug" $ \t ->
    case mkSlug t of
      Right s -> pure s
      Left e  -> fail (T.unpack e)

-- ---------------------------------------------------------------------------
-- Decoding helpers
-- ---------------------------------------------------------------------------

-- | Build a label table from an exhaustive label function.
labelTable :: (Bounded a, Enum a) => (a -> Text) -> [(Text, a)]
labelTable f = [ (f a, a) | a <- [minBound .. maxBound] ]

-- | A parser that names every acceptable value when it fails.
--
-- A typo in a spec must produce a listing, not a silent omission: a policy
-- that quietly drops its advertising disclosure is worse than one that fails
-- to load.
enumParser :: String -> [(Text, a)] -> Value -> Parser a
enumParser lbl table = withText lbl $ \t ->
  case lookup t table of
    Just a  -> pure a
    Nothing ->
      fail $ "unknown " <> lbl <> " " <> show t <> "; expected one of: "
          <> intercalate ", " (map (T.unpack . fst) table)

-- ---------------------------------------------------------------------------
-- Regions
-- ---------------------------------------------------------------------------

-- | A regime whose disclosures this document must carry.
--
-- This is a statement about distribution, not about the reader. A static page
-- cannot geolocate anyone, so region-specific content is always rendered as an
-- additive section ("Additional disclosures for residents of ...").
data Region
  = EEA
  | UK
  | Switzerland
  | CaliforniaUS
  | OtherStateUS
  | Turkey
  | RestOfWorld
  deriving (Eq, Ord, Show, Enum, Bounded)

regionLabel :: Region -> Text
regionLabel EEA          = "eea"
regionLabel UK           = "uk"
regionLabel Switzerland  = "switzerland"
regionLabel CaliforniaUS = "us-ca"
regionLabel OtherStateUS = "us-other"
regionLabel Turkey       = "turkey"
regionLabel RestOfWorld  = "rest-of-world"

-- | Prose name, for use inside generated text.
regionName :: Region -> Text
regionName EEA          = "the European Economic Area"
regionName UK           = "the United Kingdom"
regionName Switzerland  = "Switzerland"
regionName CaliforniaUS = "California"
regionName OtherStateUS = "other United States jurisdictions"
regionName Turkey       = "T\252rkiye"
regionName RestOfWorld  = "the rest of the world"

instance FromJSON Region where
  parseJSON = enumParser "region" (labelTable regionLabel)

-- ---------------------------------------------------------------------------
-- Platforms
-- ---------------------------------------------------------------------------

data Platform = IOS | Android
  deriving (Eq, Ord, Show, Enum, Bounded)

platformLabel :: Platform -> Text
platformLabel IOS     = "ios"
platformLabel Android = "android"

platformName :: Platform -> Text
platformName IOS     = "the Apple App Store"
platformName Android = "Google Play"

instance FromJSON Platform where
  parseJSON = enumParser "platform" (labelTable platformLabel)

-- ---------------------------------------------------------------------------
-- Audience
-- ---------------------------------------------------------------------------

-- | COPPA's three-way split, which drives the children's clauses and a
-- boot-time safety check on the SDK list.
data Audience
  = GeneralAudience
  | MixedAudience
  | ChildDirected
  deriving (Eq, Ord, Show, Enum, Bounded)

audienceLabel :: Audience -> Text
audienceLabel GeneralAudience = "general"
audienceLabel MixedAudience   = "mixed"
audienceLabel ChildDirected   = "child-directed"

instance FromJSON Audience where
  parseJSON = enumParser "audience" (labelTable audienceLabel)

-- ---------------------------------------------------------------------------
-- Data categories
-- ---------------------------------------------------------------------------

data DataCategory
  = DeviceIdentifiers
  | AdvertisingId
  | IpAddress
  | CoarseLocation
  | PreciseLocation
  | UsageAnalytics
  | CrashDiagnostics
  | PurchaseHistory
  | EmailAddress
  | DisplayName
  | UserContent
  | PushToken
  deriving (Eq, Ord, Show, Enum, Bounded)

dataCategoryLabel :: DataCategory -> Text
dataCategoryLabel DeviceIdentifiers = "device-identifiers"
dataCategoryLabel AdvertisingId     = "advertising-id"
dataCategoryLabel IpAddress         = "ip-address"
dataCategoryLabel CoarseLocation    = "coarse-location"
dataCategoryLabel PreciseLocation   = "precise-location"
dataCategoryLabel UsageAnalytics    = "usage-analytics"
dataCategoryLabel CrashDiagnostics  = "crash-diagnostics"
dataCategoryLabel PurchaseHistory   = "purchase-history"
dataCategoryLabel EmailAddress      = "email-address"
dataCategoryLabel DisplayName       = "display-name"
dataCategoryLabel UserContent       = "user-content"
dataCategoryLabel PushToken         = "push-token"

-- | Prose name, for the collection table.
dataCategoryName :: DataCategory -> Text
dataCategoryName DeviceIdentifiers =
  "Device identifiers (device model, operating system version, language)"
dataCategoryName AdvertisingId =
  "Advertising identifier (IDFA on iOS, Advertising ID on Android)"
dataCategoryName IpAddress        = "IP address"
dataCategoryName CoarseLocation   = "Approximate location, derived from IP address"
dataCategoryName PreciseLocation  = "Precise location"
dataCategoryName UsageAnalytics   = "Gameplay and usage events"
dataCategoryName CrashDiagnostics = "Crash reports and diagnostic logs"
dataCategoryName PurchaseHistory  = "In-app purchase history"
dataCategoryName EmailAddress     = "Email address"
dataCategoryName DisplayName      = "Display name or nickname"
dataCategoryName UserContent      = "Content you submit"
dataCategoryName PushToken        = "Push notification token"

instance FromJSON DataCategory where
  parseJSON = enumParser "data category" (labelTable dataCategoryLabel)

-- ---------------------------------------------------------------------------
-- Purposes
-- ---------------------------------------------------------------------------

data Purpose
  = ServeAds
  | MeasureAds
  | Attribution
  | Analytics
  | CrashReporting
  | CloudSave
  | Authentication
  | FraudPrevention
  | CustomerSupport
  deriving (Eq, Ord, Show, Enum, Bounded)

purposeLabel :: Purpose -> Text
purposeLabel ServeAds        = "serve-ads"
purposeLabel MeasureAds      = "measure-ads"
purposeLabel Attribution     = "attribution"
purposeLabel Analytics       = "analytics"
purposeLabel CrashReporting  = "crash-reporting"
purposeLabel CloudSave       = "cloud-save"
purposeLabel Authentication  = "authentication"
purposeLabel FraudPrevention = "fraud-prevention"
purposeLabel CustomerSupport = "customer-support"

purposeName :: Purpose -> Text
purposeName ServeAds        = "showing advertisements"
purposeName MeasureAds      = "measuring advertisement performance"
purposeName Attribution     = "attributing installs to the campaign that produced them"
purposeName Analytics       = "understanding how the game is played so we can improve it"
purposeName CrashReporting  = "diagnosing crashes and errors"
purposeName CloudSave       = "saving and restoring your game progress"
purposeName Authentication  = "signing you in"
purposeName FraudPrevention = "detecting fraud, cheating and abuse"
purposeName CustomerSupport = "answering your support requests"

instance FromJSON Purpose where
  parseJSON = enumParser "purpose" (labelTable purposeLabel)

-- ---------------------------------------------------------------------------
-- Features
-- ---------------------------------------------------------------------------

data Feature
  = CloudSaveFeature
  | AccountsFeature
  | LeaderboardsFeature
  | UserGeneratedContent
  | InGameChat
  | PushNotifications
  deriving (Eq, Ord, Show, Enum, Bounded)

featureLabel :: Feature -> Text
featureLabel CloudSaveFeature     = "cloud-save"
featureLabel AccountsFeature      = "accounts"
featureLabel LeaderboardsFeature  = "leaderboards"
featureLabel UserGeneratedContent = "user-generated-content"
featureLabel InGameChat           = "in-game-chat"
featureLabel PushNotifications    = "push-notifications"

instance FromJSON Feature where
  parseJSON = enumParser "feature" (labelTable featureLabel)

-- ---------------------------------------------------------------------------
-- Monetization
-- ---------------------------------------------------------------------------

data Monetization
  = BannerAds
  | InterstitialAds
  | RewardedAds
  | IapConsumable
  | IapNonConsumable
  | Subscription
  deriving (Eq, Ord, Show, Enum, Bounded)

monetizationLabel :: Monetization -> Text
monetizationLabel BannerAds        = "banner-ads"
monetizationLabel InterstitialAds  = "interstitial-ads"
monetizationLabel RewardedAds      = "rewarded-ads"
monetizationLabel IapConsumable    = "iap-consumable"
monetizationLabel IapNonConsumable = "iap-non-consumable"
monetizationLabel Subscription     = "subscription"

instance FromJSON Monetization where
  parseJSON = enumParser "monetization" (labelTable monetizationLabel)

-- ---------------------------------------------------------------------------
-- Refund routing
-- ---------------------------------------------------------------------------

data RefundRouting = PlatformOnly | DirectSupport
  deriving (Eq, Ord, Show, Enum, Bounded)

refundRoutingLabel :: RefundRouting -> Text
refundRoutingLabel PlatformOnly  = "platform-only"
refundRoutingLabel DirectSupport = "direct-support"

instance FromJSON RefundRouting where
  parseJSON = enumParser "refund routing" (labelTable refundRoutingLabel)
