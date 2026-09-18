{-# LANGUAGE OverloadedStrings #-}

-- | What a game declares about itself.
--
-- A spec states facts about the game and nothing else. It never names a clause
-- and never draws a legal conclusion — those belong to the corpus and the
-- catalog respectively. Keeping the boundary sharp is what stops the YAML from
-- drifting away from the documents it produces.
module Privgen.Spec
  ( GameSpec (..)
  , Company (..)
  , Representative (..)
  , StoreListing (..)
  , Retention (..)
  , TermsSpec (..)
  , strictObject
  ) where

import           Data.Aeson         (FromJSON (..), Object, Value, withObject,
                                     (.!=), (.:), (.:?))
import           Data.Aeson.Key     (Key)
import qualified Data.Aeson.Key     as K
import qualified Data.Aeson.KeyMap  as KM
import           Data.Aeson.Types   (Parser)
import           Data.List          (intercalate, (\\))
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)

import           Privgen.Catalog    (SdkId)
import           Privgen.Types

-- | Reject unknown keys.
--
-- The failure mode that actually hurts a document generator is a silently
-- ignored field: @sdsk: [admob]@ must not quietly yield a policy with no
-- advertising disclosure. aeson will not catch that on its own, so every
-- record in this module lists its keys and refuses anything else.
strictObject :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
strictObject lbl known k = withObject lbl $ \o ->
  case KM.keys o \\ known of
    []    -> k o
    extra ->
      fail $ "unknown field(s) in " <> lbl <> ": "
          <> intercalate ", " (map K.toString extra)
          <> "; known fields are: "
          <> intercalate ", " (map K.toString known)

-- ---------------------------------------------------------------------------

data GameSpec = GameSpec
  { specSlug         :: Slug
  , specName         :: Text
  , specUpdated      :: Day
  , specCompany      :: Company
  , specStores       :: [StoreListing]
  , specPlatforms    :: [Platform]
  , specAudience     :: Audience
  , specAgeGate      :: Maybe Int
  , specRegions      :: [Region]
  , specCollects     :: [DataCategory]
    -- ^ First-party collection only. What the document publishes is this
    -- unioned with the categories the declared SDKs receive, so leaving a
    -- category out here cannot cause an under-disclosure.
  , specSdks         :: [SdkId]
  , specMonetization :: [Monetization]
  , specFeatures     :: [Feature]
  , specRetention    :: Retention
  , specTerms        :: TermsSpec
  } deriving (Eq, Show)

instance FromJSON GameSpec where
  parseJSON = strictObject "game spec"
    [ "slug", "name", "updated", "company", "stores", "platforms", "audience"
    , "ageGate", "regions", "collects", "sdks", "monetization", "features"
    , "retention", "terms" ] $ \o ->
      GameSpec
        <$> o .:  "slug"
        <*> o .:  "name"
        <*> o .:  "updated"
        <*> o .:  "company"
        <*> o .:? "stores"       .!= []
        <*> o .:? "platforms"    .!= []
        <*> o .:  "audience"
        <*> o .:? "ageGate"
        <*> o .:? "regions"      .!= []
        <*> o .:? "collects"     .!= []
        <*> o .:? "sdks"         .!= []
        <*> o .:? "monetization" .!= []
        <*> o .:? "features"     .!= []
        <*> o .:  "retention"
        <*> o .:  "terms"

-- ---------------------------------------------------------------------------

-- | The controller's identity. Naming this correctly is a GDPR Art. 13 and
-- KVKK m.10 obligation, not a cosmetic detail, which is why none of it has a
-- default.
data Company = Company
  { coLegalName    :: Text
  , coAddress      :: [Text]
  , coCountry      :: Text
  , coPrivacyEmail :: Text
  , coSupportEmail :: Text
  , coWebsite      :: Maybe Text
  , coEuRep        :: Maybe Representative
    -- ^ GDPR Art. 27 representative, required of controllers outside the EEA
    -- that target it.
  , coUkRep        :: Maybe Representative
    -- ^ UK GDPR Art. 27 representative.
  , coVerbis       :: Maybe Text
    -- ^ VERBIS registration number, where the controller is registered in
    -- Turkiye.
  } deriving (Eq, Show)

instance FromJSON Company where
  parseJSON = strictObject "company"
    [ "legalName", "address", "country", "privacyEmail", "supportEmail"
    , "website", "euRepresentative", "ukRepresentative", "verbis" ] $ \o ->
      Company
        <$> o .:  "legalName"
        <*> o .:? "address" .!= []
        <*> o .:  "country"
        <*> o .:  "privacyEmail"
        <*> o .:  "supportEmail"
        <*> o .:? "website"
        <*> o .:? "euRepresentative"
        <*> o .:? "ukRepresentative"
        <*> o .:? "verbis"

data Representative = Representative
  { repName    :: Text
  , repAddress :: [Text]
  , repEmail   :: Text
  } deriving (Eq, Show)

instance FromJSON Representative where
  parseJSON = strictObject "representative" ["name", "address", "email"] $ \o ->
    Representative
      <$> o .:  "name"
      <*> o .:? "address" .!= []
      <*> o .:  "email"

-- ---------------------------------------------------------------------------

data StoreListing = StoreListing
  { stPlatform :: Platform
  , stBundleId :: Text
  , stUrl      :: Text
  } deriving (Eq, Show)

instance FromJSON StoreListing where
  parseJSON = strictObject "store listing" ["platform", "bundleId", "url"] $ \o ->
    StoreListing
      <$> o .: "platform"
      <*> o .: "bundleId"
      <*> o .: "url"

-- ---------------------------------------------------------------------------

-- | Retention periods, as free text so they can read naturally ("14 months",
-- "for as long as your account exists"). GDPR Art. 13(2)(a) wants a period or
-- the criteria used to determine one; the amended COPPA Rule requires a
-- written retention policy for children's data.
data Retention = Retention
  { retAnalytics :: Text
  , retCrash     :: Text
  , retSupport   :: Text
  , retAccount   :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON Retention where
  parseJSON = strictObject "retention"
    ["analytics", "crash", "support", "account"] $ \o ->
      Retention
        <$> o .:  "analytics"
        <*> o .:  "crash"
        <*> o .:  "support"
        <*> o .:? "account"

-- ---------------------------------------------------------------------------

data TermsSpec = TermsSpec
  { tsGoverningLaw :: Text
  , tsVenue        :: Text
  , tsArbitration  :: Bool
    -- ^ US binding arbitration with a class-action waiver. Off unless chosen
    -- deliberately: it is a policy decision with real consequences for
    -- players, not a default.
  , tsMinAge       :: Int
  , tsRefunds      :: RefundRouting
  , tsVirtualItems :: Bool
  } deriving (Eq, Show)

instance FromJSON TermsSpec where
  parseJSON = strictObject "terms"
    [ "governingLaw", "venue", "arbitration", "minAge", "refunds"
    , "virtualItems" ] $ \o ->
      TermsSpec
        <$> o .:  "governingLaw"
        <*> o .:  "venue"
        <*> o .:? "arbitration"  .!= False
        <*> o .:  "minAge"
        <*> o .:  "refunds"
        <*> o .:? "virtualItems" .!= False
