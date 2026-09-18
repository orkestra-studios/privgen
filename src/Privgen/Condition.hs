{-# LANGUAGE OverloadedStrings #-}

-- | When a clause applies.
--
-- Conditions are data, not predicates. That costs a few lines per clause and
-- buys three things a @Spec -> Bool@ cannot give: selection can be tested
-- without rendering anything, the whole corpus can be analysed statically for
-- gaps (see "Privgen.Document"), and every decision can be explained to a
-- human reviewer.
--
-- The type is deliberately closed. Adding a @Custom Text (Env -> Bool)@
-- constructor would restore the convenience of predicates and immediately
-- blind the corpus lint, which is the main reason this design exists. When a
-- clause needs something the vocabulary cannot say, add an atom.
module Privgen.Condition
  ( Condition (..)
  , Trace (..)
  , explain
  , evalCondition
  , conditionAtoms
  , renderTrace
  ) where

import           Data.Text       (Text)
import qualified Data.Text       as T

import           Privgen.Catalog
import           Privgen.Env
import           Privgen.Spec
import           Privgen.Types

data Condition
  = Always
  | Never
  | InRegion Region
  | ForAudience Audience
  | OnPlatform Platform
  | UsesSdk SdkId
  | AnySdkWith SdkTrait
    -- ^ Asks the catalog about a class of vendor rather than naming one. This
    -- is the bridge that lets one catalog entry drive four clauses.
  | Collects DataCategory
  | ServesPurpose Purpose
  | HasFeature Feature
  | Monetizes Monetization
  | DeclaresAds
  | DeclaresIap
  | ArbitrationElected
  | HasVirtualItems
  | HasEuRepresentative
  | HasUkRepresentative
  | HasVerbisNumber
  | Not Condition
  | AllOf [Condition]
  | AnyOf [Condition]
  deriving (Eq, Show)

-- | Why a condition came out the way it did, one node per sub-condition.
data Trace = Trace
  { trCondition :: Condition
  , trResult    :: Bool
  , trWhy       :: Text
  , trSub       :: [Trace]
  } deriving (Eq, Show)

-- | Defined in terms of 'explain' so the audit trail and the actual behaviour
-- cannot drift apart. There is only one implementation.
evalCondition :: Env -> Condition -> Bool
evalCondition env = trResult . explain env

explain :: Env -> Condition -> Trace
explain env = go
  where
    spec = envSpec env

    leaf c r why = Trace c r why []

    go Always = leaf Always True "unconditional"
    go Never  = leaf Never False "staged, never selected"

    go c@(InRegion r) =
      leaf c (r `elem` specRegions spec) $
        "regions = " <> listOf regionLabel (specRegions spec)

    go c@(ForAudience a) =
      leaf c (specAudience spec == a) $
        "audience = " <> audienceLabel (specAudience spec)

    go c@(OnPlatform p) =
      leaf c (p `elem` specPlatforms spec) $
        "platforms = " <> listOf platformLabel (specPlatforms spec)

    go c@(UsesSdk i) =
      leaf c (i `elem` specSdks spec) $
        "sdks = " <> listOf sdkIdLabel (specSdks spec)

    go c@(AnySdkWith t) =
      let matching = filter (sdkHasTrait t) (envSdks env)
      in leaf c (not (null matching)) $
           T.pack (show t) <> " matched by "
             <> (if null matching
                   then "no listed sdk"
                   else listOf sdkIdLabel (map sdkId matching))

    go c@(Collects cat) =
      leaf c (cat `elem` collectedList) $
        "collected (first-party and via sdks) = "
          <> listOf dataCategoryLabel collectedList

    go c@(ServesPurpose p) =
      leaf c (p `elem` purposeList) $
        "purposes = " <> listOf purposeLabel purposeList

    go c@(HasFeature f) =
      leaf c (f `elem` specFeatures spec) $
        "features = " <> listOf featureLabel (specFeatures spec)

    go c@(Monetizes m) =
      leaf c (m `elem` specMonetization spec) $
        "monetization = " <> listOf monetizationLabel (specMonetization spec)

    go c@DeclaresAds =
      leaf c (declaresAds env) $
        "monetization = " <> listOf monetizationLabel (specMonetization spec)

    go c@DeclaresIap =
      leaf c (declaresIap env) $
        "monetization = " <> listOf monetizationLabel (specMonetization spec)

    go c@ArbitrationElected =
      leaf c (tsArbitration (specTerms spec)) "terms.arbitration"

    go c@HasVirtualItems =
      leaf c (tsVirtualItems (specTerms spec)) "terms.virtualItems"

    go c@HasEuRepresentative =
      leaf c (present (coEuRep (specCompany spec))) "company.euRepresentative"

    go c@HasUkRepresentative =
      leaf c (present (coUkRep (specCompany spec))) "company.ukRepresentative"

    go c@HasVerbisNumber =
      leaf c (present (coVerbis (specCompany spec))) "company.verbis"

    go c@(Not inner) =
      let t = go inner
      in Trace c (not (trResult t)) "negation" [t]

    go c@(AllOf cs) =
      let ts = map go cs
      in Trace c (all trResult ts) "all of" ts

    go c@(AnyOf cs) =
      let ts = map go cs
      in Trace c (any trResult ts) "any of" ts

    collectedList = setToList (envCollected env)
    purposeList   = setToList (envPurposes env)

    setToList = foldr (:) []

    present :: Maybe a -> Bool
    present (Just _) = True
    present Nothing  = False

listOf :: (a -> Text) -> [a] -> Text
listOf f xs
  | null xs   = "(none)"
  | otherwise = T.intercalate ", " (map f xs)

-- | The leaves of a condition. Used by the corpus lint to work out which parts
-- of the vocabulary no clause ever mentions — that is, which disclosure
-- someone forgot to write.
conditionAtoms :: Condition -> [Condition]
conditionAtoms c = case c of
  Not inner -> conditionAtoms inner
  AllOf cs  -> concatMap conditionAtoms cs
  AnyOf cs  -> concatMap conditionAtoms cs
  atom      -> [atom]

-- | Flatten a trace for the audit file.
renderTrace :: Trace -> [Text]
renderTrace = go 0
  where
    go depth t =
      (T.replicate depth "  " <> mark (trResult t) <> " "
         <> T.pack (showShallow (trCondition t)) <> "  -- " <> trWhy t)
        : concatMap (go (depth + 1)) (trSub t)

    mark True  = "[x]"
    mark False = "[ ]"

-- | Show a condition without dumping its whole subtree, which the nested
-- trace lines already cover.
showShallow :: Condition -> String
showShallow (Not _)     = "Not"
showShallow (AllOf cs)  = "AllOf (" <> show (length cs) <> ")"
showShallow (AnyOf cs)  = "AnyOf (" <> show (length cs) <> ")"
showShallow c           = show c
