{-# LANGUAGE OverloadedStrings #-}

-- | Semantic checks that decoding cannot express.
--
-- The important export is 'ValidSpec', whose constructor is deliberately not
-- exported. Every renderer takes a 'ValidSpec', so a spec that has not passed
-- these checks cannot reach a document. That is the structural replacement for
-- the old partial @read@, which deferred its failure until the response was
-- already being produced.
--
-- Checks accumulate: one run reports everything wrong with a file, because
-- fixing specs one error per build is miserable.
module Privgen.Validate
  ( ValidSpec
  , unValid
  , validateSpec
  , SpecError (..)
  , Severity (..)
  , severityOf
  , renderSpecError
  ) where

import           Data.List       (nub, sort)
import           Data.Maybe      (isNothing)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Privgen.Catalog
import           Privgen.Spec
import           Privgen.Types

-- | A 'GameSpec' that has passed 'validateSpec'.
newtype ValidSpec = ValidSpec { unValid :: GameSpec }
  deriving (Eq, Show)

data SpecError
  = EmptyRegions
  | EmptyPlatforms
  | DuplicateSdks [SdkId]
  | ChildDirectedSdkNotChildSafe SdkId
  | ChildDirectedSellingSdk SdkId
  | ChildDirectedPreciseLocation
  | ChildDirectedArbitration
  | AdsDeclaredButNoAdSdk
  | AdSdkButNoAdMonetization SdkId
  | MissingEuRepresentative
  | MissingUkRepresentative
  | AgeGateBelowTermsMinimum Int Int
  | StoreListingForUndeclaredPlatform Platform
  | UserContentWithoutFeature
  deriving (Eq, Show)

-- | Whether a finding stops the process or merely warns.
--
-- The split exists because not every gap is the same kind of problem. Shipping
-- a behavioural ad SDK in a child-directed game is a defect in the spec and
-- must not start. Not having appointed an Art. 27 representative is a gap in
-- the business, not in the file — refusing to serve any document over it would
-- take a live policy URL offline to protest a paperwork omission, which helps
-- nobody. Advisory findings are printed loudly at every boot instead.
data Severity = Blocking | Advisory
  deriving (Eq, Show)

severityOf :: SpecError -> Severity
severityOf MissingEuRepresentative = Advisory
severityOf MissingUkRepresentative = Advisory
severityOf EmptyRegions                       = Blocking
severityOf EmptyPlatforms                     = Blocking
severityOf (DuplicateSdks _)                  = Blocking
severityOf (ChildDirectedSdkNotChildSafe _)   = Blocking
severityOf (ChildDirectedSellingSdk _)        = Blocking
severityOf ChildDirectedPreciseLocation       = Blocking
severityOf ChildDirectedArbitration           = Blocking
severityOf AdsDeclaredButNoAdSdk              = Blocking
severityOf (AdSdkButNoAdMonetization _)       = Blocking
severityOf (AgeGateBelowTermsMinimum _ _)     = Blocking
severityOf (StoreListingForUndeclaredPlatform _) = Blocking
severityOf UserContentWithoutFeature          = Blocking

renderSpecError :: SpecError -> Text
renderSpecError EmptyRegions =
  "regions: must name at least one region; a document with no regime has \
  \nothing to disclose"
renderSpecError EmptyPlatforms =
  "platforms: must name at least one platform"
renderSpecError (DuplicateSdks ids) =
  "sdks: repeated entries: " <> T.intercalate ", " (map sdkIdLabel ids)
renderSpecError (ChildDirectedSdkNotChildSafe i) =
  "audience is child-directed, but " <> sdkIdLabel i
    <> " has no child-directed mode. Under COPPA this SDK must not ship in a \
       \child-directed title."
renderSpecError (ChildDirectedSellingSdk i) =
  "audience is child-directed, but " <> sdkIdLabel i
    <> " sells or shares personal information for behavioural advertising. \
       \The amended COPPA Rule requires separate verifiable parental consent \
       \for third-party disclosure, which this configuration cannot satisfy."
renderSpecError ChildDirectedPreciseLocation =
  "audience is child-directed, but collects precise-location"
renderSpecError ChildDirectedArbitration =
  "audience is child-directed, but terms.arbitration is true; a class-action \
  \waiver against children is not defensible"
renderSpecError AdsDeclaredButNoAdSdk =
  "monetization declares advertising, but no advertising SDK is listed"
renderSpecError (AdSdkButNoAdMonetization i) =
  "sdks lists " <> sdkIdLabel i
    <> ", which serves advertising, but monetization declares no ad formats"
renderSpecError MissingEuRepresentative =
  "regions includes eea and the controller is established outside it, so \
  \company.euRepresentative is required (GDPR Art. 27)"
renderSpecError MissingUkRepresentative =
  "regions includes uk and the controller is established outside it, so \
  \company.ukRepresentative is required (UK GDPR Art. 27)"
renderSpecError (AgeGateBelowTermsMinimum gate minAge) =
  "ageGate is " <> tshow gate <> " but terms.minAge is " <> tshow minAge
    <> "; the age gate cannot admit players the terms exclude"
renderSpecError (StoreListingForUndeclaredPlatform p) =
  "stores lists a " <> platformLabel p <> " listing, but platforms does not \
  \include it"
renderSpecError UserContentWithoutFeature =
  "collects includes user-content, but features declares neither \
  \user-generated-content nor in-game-chat"

tshow :: Int -> Text
tshow = T.pack . show

-- | Run every check.
--
-- 'Left' carries every blocking failure, not just the first. 'Right' carries
-- the validated spec together with any advisory findings, which the caller is
-- expected to surface rather than swallow.
validateSpec :: GameSpec -> Either [SpecError] (ValidSpec, [SpecError])
validateSpec s =
  case filter isBlocking findings of
    [] -> Right (ValidSpec s, filter (not . isBlocking) findings)
    es -> Left es
  where
    findings   = concat checks
    isBlocking = (== Blocking) . severityOf

    sdks      = specSdks s
    entries   = map sdkEntry sdks
    regions   = specRegions s
    audience  = specAudience s
    terms     = specTerms s
    company   = specCompany s
    monet     = specMonetization s

    adFormats   = [BannerAds, InterstitialAds, RewardedAds]
    declaresAds = any (`elem` adFormats) monet
    adSdks      = filter (Set.member ServeAds . sdkPurposes) entries

    checks =
      [ [ EmptyRegions   | null regions ]
      , [ EmptyPlatforms | null (specPlatforms s) ]
      , [ DuplicateSdks dups | not (null dups) ]
      , childChecks
      , adChecks
      , repChecks
      , ageChecks
      , storeChecks
      , ugcChecks
      ]

    dups = sort (nub [ i | i <- sdks, count i sdks > 1 ])
    count x = length . filter (== x)

    childChecks
      | audience /= ChildDirected = []
      | otherwise = concat
          [ [ ChildDirectedSdkNotChildSafe (sdkId e)
            | e <- entries, sdkChildSafety e == NoChildMode ]
          , [ ChildDirectedSellingSdk (sdkId e)
            | e <- entries, sdkSellsShares e ]
          , [ ChildDirectedPreciseLocation
            | PreciseLocation `elem` specCollects s ]
          , [ ChildDirectedArbitration | tsArbitration terms ]
          ]

    adChecks = concat
      [ [ AdsDeclaredButNoAdSdk | declaresAds, null adSdks ]
      , [ AdSdkButNoAdMonetization (sdkId e)
        | not declaresAds, e <- adSdks ]
      ]

    -- Art. 27 bites on controllers established outside the territory that
    -- nonetheless target it.
    repChecks = concat
      [ [ MissingEuRepresentative
        | EEA `elem` regions
        , not (isEeaCountry (coCountry company))
        , isNothing (coEuRep company) ]
      , [ MissingUkRepresentative
        | UK `elem` regions
        , not (isUkCountry (coCountry company))
        , isNothing (coUkRep company) ]
      ]

    ageChecks =
      [ AgeGateBelowTermsMinimum gate (tsMinAge terms)
      | Just gate <- [specAgeGate s], gate < tsMinAge terms ]

    storeChecks =
      [ StoreListingForUndeclaredPlatform p
      | p <- nub (map stPlatform (specStores s))
      , p `notElem` specPlatforms s ]

    ugcChecks =
      [ UserContentWithoutFeature
      | UserContent `elem` specCollects s
      , UserGeneratedContent `notElem` specFeatures s
      , InGameChat `notElem` specFeatures s ]

-- | Free-text country matching, deliberately forgiving. A false negative here
-- only ever asks for a representative that may not be needed; a false positive
-- would silently drop a required one.
isEeaCountry :: Text -> Bool
isEeaCountry c = normalise c `elem` eeaCountries

isUkCountry :: Text -> Bool
isUkCountry c = normalise c `elem`
  ["united kingdom", "uk", "great britain", "england", "scotland", "wales"]

normalise :: Text -> Text
normalise = T.strip . T.toLower

eeaCountries :: [Text]
eeaCountries =
  [ "austria", "belgium", "bulgaria", "croatia", "cyprus", "czechia"
  , "czech republic", "denmark", "estonia", "finland", "france", "germany"
  , "greece", "hungary", "iceland", "ireland", "italy", "latvia"
  , "liechtenstein", "lithuania", "luxembourg", "malta", "netherlands"
  , "norway", "poland", "portugal", "romania", "slovakia", "slovenia"
  , "spain", "sweden"
  ]
