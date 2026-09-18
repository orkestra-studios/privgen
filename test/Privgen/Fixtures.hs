{-# LANGUAGE OverloadedStrings #-}

-- | Test specs, built through 'validateSpec' rather than by hand.
--
-- Going through the real validator means a fixture cannot drift away from what
-- the loader would actually accept — a fixture that the production path would
-- reject is worse than no fixture at all.
module Privgen.Fixtures
  ( baseSpec
  , validOrDie
  , envFor
  , fixedDay
  , euUsAdSupported
  , usOnlyNoAds
  , childDirected
  , turkeyOnly
  ) where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day, fromGregorian)

import           Privgen.Catalog
import           Privgen.Env
import           Privgen.Spec
import           Privgen.Types
import           Privgen.Validate

-- | Frozen so golden output never depends on when the suite runs.
fixedDay :: Day
fixedDay = fromGregorian 2026 9 19

baseSpec :: GameSpec
baseSpec = GameSpec
  { specSlug         = slugOrDie "test-game"
  , specName         = "Test Game"
  , specUpdated      = fixedDay
  , specCompany      = baseCompany
  , specStores       = []
  , specPlatforms    = [IOS, Android]
  , specAudience     = GeneralAudience
  , specAgeGate      = Just 13
  , specRegions      = [RestOfWorld]
  , specCollects     = [DeviceIdentifiers, UsageAnalytics]
  , specSdks         = [GameAnalytics]
  , specMonetization = []
  , specFeatures     = []
  , specRetention    = baseRetention
  , specTerms        = baseTerms
  }

baseCompany :: Company
baseCompany = Company
  { coLegalName    = "Test Company Ltd"
  , coAddress      = ["1 Test Street", "Testville"]
  , coCountry      = "Ireland"
    -- Inside the EEA, so the Art. 27 advisory does not fire for fixtures that
    -- do not care about it.
  , coPrivacyEmail = "privacy@example.com"
  , coSupportEmail = "support@example.com"
  , coWebsite      = Just "https://example.com"
  , coEuRep        = Nothing
  , coUkRep        = Nothing
  , coVerbis       = Nothing
  }

baseRetention :: Retention
baseRetention = Retention
  { retAnalytics = "14 months"
  , retCrash     = "90 days"
  , retSupport   = "3 years"
  , retAccount   = Nothing
  }

baseTerms :: TermsSpec
baseTerms = TermsSpec
  { tsGoverningLaw = "Ireland"
  , tsVenue        = "Dublin"
  , tsArbitration  = False
  , tsMinAge       = 13
  , tsRefunds      = PlatformOnly
  , tsVirtualItems = False
  }

slugOrDie :: Text -> Slug
slugOrDie s = case mkSlug s of
  Right v -> v
  Left e  -> error ("fixture slug rejected: " <> T.unpack e)

validOrDie :: GameSpec -> ValidSpec
validOrDie s = case validateSpec s of
  Right (v, _) -> v
  Left es      -> error ("fixture rejected by validateSpec: " <> show es)

envFor :: GameSpec -> Env
envFor = mkEnv fixedDay . validOrDie

-- ---------------------------------------------------------------------------
-- Scenarios
-- ---------------------------------------------------------------------------

-- | Europe and the US, ad supported. The common shape.
euUsAdSupported :: GameSpec
euUsAdSupported = baseSpec
  { specRegions      = [EEA, UK, CaliforniaUS, OtherStateUS]
  , specSdks         = [AppLovinMax, GameAnalytics, Adjust, Crashlytics]
  , specMonetization = [BannerAds, InterstitialAds, RewardedAds, IapConsumable]
  , specCollects     = [DeviceIdentifiers, AdvertisingId, UsageAnalytics]
  , specTerms        = baseTerms { tsVirtualItems = True }
  }

-- | US only, no advertising at all. Nothing European should appear.
usOnlyNoAds :: GameSpec
usOnlyNoAds = baseSpec
  { specRegions      = [CaliforniaUS, OtherStateUS]
  , specSdks         = [GameAnalytics, Crashlytics]
  , specMonetization = []
  }

-- | Child-directed. Behavioural advertising must disappear; COPPA clauses
-- must appear.
childDirected :: GameSpec
childDirected = baseSpec
  { specAudience     = ChildDirected
  , specRegions      = [CaliforniaUS, OtherStateUS, EEA]
  , specSdks         = [GameAnalytics, Crashlytics]
  , specMonetization = []
  }

turkeyOnly :: GameSpec
turkeyOnly = baseSpec
  { specRegions = [Turkey]
  , specCompany = baseCompany { coCountry = "Turkiye"
                              , coVerbis  = Just "1234567" }
  }
