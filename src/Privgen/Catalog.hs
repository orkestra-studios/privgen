{-# LANGUAGE OverloadedStrings #-}

-- | Facts about third-party SDKs, in one place.
--
-- A game spec names SDKs; it never restates what they do. "AdMob receives an
-- advertising identifier, and that counts as a sale/share under the CCPA" is a
-- fact about AdMob, so it lives here. If each game file restated it, the files
-- would drift and the policies would start lying.
--
-- One entry feeds four different obligations: the GDPR recipients list, the
-- CCPA sale/share disclosure, the COPPA third-party-disclosure check, and the
-- KVKK international transfer clause.
--
-- 'sdkEntry' is a total function rather than a @Map@ on purpose. Compiled with
-- @-Wall@, adding a constructor to 'SdkId' breaks the build until every legal
-- fact about it has been supplied. A @Map@ would hand you a runtime @Nothing@
-- instead, and a missing recipient is exactly the kind of omission that makes
-- a policy inaccurate.
module Privgen.Catalog
  ( SdkId (..)
  , SdkEntry (..)
  , ProcessorRole (..)
  , ChildSafety (..)
  , TransferBasis (..)
  , SdkTrait (..)
  , sdkEntry
  , allSdks
  , allSdkIds
  , sdkIdLabel
  , sdkHasTrait
  , transferBasisName
  ) where

import           Data.Aeson         (FromJSON (..))
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)

import           Privgen.Types

-- | Every SDK any Orkestra title has shipped.
data SdkId
  = AppLovinMax
  | UnityAds
  | IronSource
  | AdMob
  | MetaAudienceNetwork
  | GameAnalytics
  | Adjust
  | AppsFlyer
  | FirebaseAnalytics
  | Crashlytics
  | VoodooPublishing
  | AraratGames
  deriving (Eq, Ord, Show, Enum, Bounded)

sdkIdLabel :: SdkId -> Text
sdkIdLabel AppLovinMax         = "applovin-max"
sdkIdLabel UnityAds            = "unity-ads"
sdkIdLabel IronSource          = "ironsource"
sdkIdLabel AdMob               = "admob"
sdkIdLabel MetaAudienceNetwork = "meta-audience-network"
sdkIdLabel GameAnalytics       = "gameanalytics"
sdkIdLabel Adjust              = "adjust"
sdkIdLabel AppsFlyer           = "appsflyer"
sdkIdLabel FirebaseAnalytics   = "firebase-analytics"
sdkIdLabel Crashlytics         = "crashlytics"
sdkIdLabel VoodooPublishing    = "voodoo"
sdkIdLabel AraratGames         = "ararat-games"

instance FromJSON SdkId where
  parseJSON = enumParser "sdk" (labelTable sdkIdLabel)

-- | How the vendor acts with respect to the data it receives. Processors act
-- on our instructions; controllers decide for themselves, which is why their
-- own policies and contacts have to be surfaced to the reader.
data ProcessorRole
  = Processor
  | IndependentController
  | JointController
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Whether the SDK can be operated in a mode fit for a child-directed game.
data ChildSafety
  = NoChildMode
  | ChildDirectedFlag
  | CertifiedKidsSafe
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The mechanism relied on to move data out of a protected jurisdiction.
data TransferBasis
  = Adequacy
  | StandardContractualClauses
  | UkIdta
  | TurkishStandardContract
  deriving (Eq, Ord, Show, Enum, Bounded)

transferBasisName :: TransferBasis -> Text
transferBasisName Adequacy =
  "an adequacy decision covering the destination country"
transferBasisName StandardContractualClauses =
  "the European Commission's Standard Contractual Clauses"
transferBasisName UkIdta =
  "the UK International Data Transfer Addendum"
transferBasisName TurkishStandardContract =
  "a standard contract notified to the Turkish Data Protection Authority"

-- | A derived property, so conditions can ask about a class of SDK rather than
-- naming each one. Corpus clauses use these instead of enumerating vendors.
data SdkTrait
  = SellsOrShares
  | ActsAsController
  | IsProcessor
  | TransfersInternationally
  | HasChildMode
  deriving (Eq, Ord, Show, Enum, Bounded)

data SdkEntry = SdkEntry
  { sdkId          :: SdkId
  , sdkName        :: Text
    -- ^ Product name, as a reader would recognise it.
  , sdkVendor      :: Text
    -- ^ Legal entity. This is what a GDPR recipients list has to name.
  , sdkPrivacyUrl  :: Text
  , sdkOptOutUrl   :: Maybe Text
    -- ^ Where a reader exercises a CCPA opt-out against this vendor.
  , sdkContact     :: Maybe Text
  , sdkPurposes    :: Set Purpose
  , sdkReceives    :: Set DataCategory
  , sdkRole        :: ProcessorRole
  , sdkSellsShares :: Bool
    -- ^ Sale, or sharing for cross-context behavioural advertising, under the
    -- CCPA as amended by the CPRA.
  , sdkChildSafety :: ChildSafety
  , sdkTransfers   :: [TransferBasis]
  } deriving (Eq, Show)

sdkHasTrait :: SdkTrait -> SdkEntry -> Bool
sdkHasTrait SellsOrShares            = sdkSellsShares
sdkHasTrait ActsAsController         = (/= Processor) . sdkRole
sdkHasTrait IsProcessor              = (== Processor) . sdkRole
sdkHasTrait TransfersInternationally = not . null . sdkTransfers
sdkHasTrait HasChildMode             = (/= NoChildMode) . sdkChildSafety

allSdkIds :: [SdkId]
allSdkIds = [minBound .. maxBound]

-- | Catalog order. Everything downstream derives its ordering from this, never
-- from the order SDKs happen to appear in a YAML file, so reordering a spec
-- cannot churn a rendered document.
allSdks :: [SdkEntry]
allSdks = map sdkEntry allSdkIds

sdkEntry :: SdkId -> SdkEntry

sdkEntry AppLovinMax = SdkEntry
  { sdkId          = AppLovinMax
  , sdkName        = "AppLovin MAX"
  , sdkVendor      = "AppLovin Corporation"
  , sdkPrivacyUrl  = "https://www.applovin.com/privacy/"
  , sdkOptOutUrl   = Just "https://www.applovin.com/optout/"
  , sdkContact     = Just "privacy@applovin.com"
  , sdkPurposes    = Set.fromList [ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress, CoarseLocation]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry UnityAds = SdkEntry
  { sdkId          = UnityAds
  , sdkName        = "Unity Ads"
  , sdkVendor      = "Unity Technologies SF"
  , sdkPrivacyUrl  = "https://unity.com/legal/privacy-policy"
  , sdkOptOutUrl   = Just "https://unity.com/legal/privacy-policy"
  , sdkContact     = Just "dpo@unity3d.com"
  , sdkPurposes    = Set.fromList [ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress, CoarseLocation]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry IronSource = SdkEntry
  { sdkId          = IronSource
  , sdkName        = "ironSource"
  , sdkVendor      = "ironSource Ltd. (Unity Technologies)"
  , sdkPrivacyUrl  = "https://unity.com/legal/privacy-policy"
  , sdkOptOutUrl   = Just "https://unity.com/legal/privacy-policy"
  , sdkContact     = Just "dpo@unity3d.com"
  , sdkPurposes    = Set.fromList [ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress, CoarseLocation]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry AdMob = SdkEntry
  { sdkId          = AdMob
  , sdkName        = "Google AdMob"
  , sdkVendor      = "Google Ireland Limited and Google LLC"
  , sdkPrivacyUrl  = "https://policies.google.com/privacy"
  , sdkOptOutUrl   = Just "https://adssettings.google.com/"
  , sdkContact     = Nothing
  , sdkPurposes    = Set.fromList [ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress, CoarseLocation]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry MetaAudienceNetwork = SdkEntry
  { sdkId          = MetaAudienceNetwork
  , sdkName        = "Meta Audience Network"
  , sdkVendor      = "Meta Platforms Ireland Limited"
  , sdkPrivacyUrl  = "https://www.facebook.com/about/privacy"
  , sdkOptOutUrl   = Just "https://www.facebook.com/settings?tab=ads"
  , sdkContact     = Nothing
  , sdkPurposes    = Set.fromList [ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = NoChildMode
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry GameAnalytics = SdkEntry
  { sdkId          = GameAnalytics
  , sdkName        = "GameAnalytics"
  , sdkVendor      = "GameAnalytics ApS"
  , sdkPrivacyUrl  = "https://gameanalytics.com/privacy"
  , sdkOptOutUrl   = Nothing
  , sdkContact     = Just "privacy@gameanalytics.com"
  , sdkPurposes    = Set.fromList [Analytics]
  , sdkReceives    = Set.fromList [DeviceIdentifiers, UsageAnalytics, IpAddress, CoarseLocation]
  , sdkRole        = Processor
  , sdkSellsShares = False
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [Adequacy]
  }

sdkEntry Adjust = SdkEntry
  { sdkId          = Adjust
  , sdkName        = "Adjust"
  , sdkVendor      = "Adjust GmbH"
  , sdkPrivacyUrl  = "https://www.adjust.com/terms/privacy-policy"
  , sdkOptOutUrl   = Just "https://www.adjust.com/opt-out/"
  , sdkContact     = Just "privacy@adjust.com"
  , sdkPurposes    = Set.fromList [Attribution, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress]
  , sdkRole        = Processor
  , sdkSellsShares = False
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [Adequacy]
  }

sdkEntry AppsFlyer = SdkEntry
  { sdkId          = AppsFlyer
  , sdkName        = "AppsFlyer"
  , sdkVendor      = "AppsFlyer Ltd."
  , sdkPrivacyUrl  = "https://www.appsflyer.com/legal/services-privacy-policy/"
  , sdkOptOutUrl   = Just "https://www.appsflyer.com/legal/opt-out/"
  , sdkContact     = Just "privacy@appsflyer.com"
  , sdkPurposes    = Set.fromList [Attribution, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, IpAddress]
  , sdkRole        = Processor
  , sdkSellsShares = False
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses]
  }

sdkEntry FirebaseAnalytics = SdkEntry
  { sdkId          = FirebaseAnalytics
  , sdkName        = "Google Analytics for Firebase"
  , sdkVendor      = "Google Ireland Limited and Google LLC"
  , sdkPrivacyUrl  = "https://policies.google.com/privacy"
  , sdkOptOutUrl   = Nothing
  , sdkContact     = Nothing
  , sdkPurposes    = Set.fromList [Analytics]
  , sdkReceives    = Set.fromList [DeviceIdentifiers, UsageAnalytics, IpAddress, CoarseLocation]
  , sdkRole        = Processor
  , sdkSellsShares = False
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

sdkEntry Crashlytics = SdkEntry
  { sdkId          = Crashlytics
  , sdkName        = "Firebase Crashlytics"
  , sdkVendor      = "Google Ireland Limited and Google LLC"
  , sdkPrivacyUrl  = "https://policies.google.com/privacy"
  , sdkOptOutUrl   = Nothing
  , sdkContact     = Nothing
  , sdkPurposes    = Set.fromList [CrashReporting]
  , sdkReceives    = Set.fromList [DeviceIdentifiers, CrashDiagnostics, IpAddress]
  , sdkRole        = Processor
  , sdkSellsShares = False
  , sdkChildSafety = ChildDirectedFlag
  , sdkTransfers   = [StandardContractualClauses, UkIdta]
  }

-- Publisher partners. They are not libraries in the usual sense, but they do
-- receive data and decide their own purposes, so a reader has to be told about
-- them on the same footing as an ad network.
sdkEntry VoodooPublishing = SdkEntry
  { sdkId          = VoodooPublishing
  , sdkName        = "Voodoo"
  , sdkVendor      = "Voodoo SAS"
  , sdkPrivacyUrl  = "https://www.voodoo.io/privacy"
  , sdkOptOutUrl   = Nothing
  , sdkContact     = Just "dpo@voodoo.io"
  , sdkPurposes    = Set.fromList [Analytics, ServeAds, MeasureAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, UsageAnalytics, IpAddress]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = NoChildMode
  , sdkTransfers   = [StandardContractualClauses]
  }

sdkEntry AraratGames = SdkEntry
  { sdkId          = AraratGames
  , sdkName        = "Ararat Games"
  , sdkVendor      = "OOO DEVGEIM"
  , sdkPrivacyUrl  = "https://devgame.me/policy"
  , sdkOptOutUrl   = Nothing
  , sdkContact     = Just "policy@devgame.me"
  , sdkPurposes    = Set.fromList [Analytics, ServeAds]
  , sdkReceives    = Set.fromList [AdvertisingId, DeviceIdentifiers, UsageAnalytics, IpAddress]
  , sdkRole        = IndependentController
  , sdkSellsShares = True
  , sdkChildSafety = NoChildMode
  , sdkTransfers   = [StandardContractualClauses]
  }
