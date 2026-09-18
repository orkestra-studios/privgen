{-# LANGUAGE OverloadedStrings #-}

-- | Everything a clause is allowed to look at, plus the facts derived from it.
--
-- Derived facts live here rather than inside clause bodies so that tests can
-- assert on them directly. The recipient list, the sale/share list and the
-- collection table are structured data; only prose belongs in the corpus.
module Privgen.Env
  ( Env (..)
  , mkEnv
    -- * Derived facts
  , recipients
  , processors
  , controllers
  , saleOrShareSdks
  , transferSdks
  , transferBases
  , collectionTable
  , declaresAds
  , declaresIap
  , adFormats
  , iapFormats
  ) where

import           Data.List          (nub)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Time.Calendar (Day)

import           Privgen.Catalog
import           Privgen.Spec
import           Privgen.Types
import           Privgen.Validate

data Env = Env
  { envSpec      :: GameSpec
  , envSdks      :: [SdkEntry]
    -- ^ Resolved from the spec, in catalog order. Never in the order the YAML
    -- happened to list them, so reordering a spec cannot churn a document.
  , envCollected :: Set DataCategory
    -- ^ First-party collection unioned with everything the SDKs receive.
  , envPurposes  :: Set Purpose
  , envToday     :: Day
    -- ^ Injected, never read from the clock inside a renderer. A clause that
    -- called 'Data.Time.getCurrentTime' would rot every golden file overnight.
  }

mkEnv :: Day -> ValidSpec -> Env
mkEnv today vs = Env
  { envSpec      = spec
  , envSdks      = sdks
  , envCollected = firstParty `Set.union` fromSdks
  , envPurposes  = Set.unions (map sdkPurposes sdks)
  , envToday     = today
  }
  where
    spec       = unValid vs
    sdks       = filter (\e -> sdkId e `elem` specSdks spec) allSdks
    firstParty = Set.fromList (specCollects spec)
    fromSdks   = Set.unions (map sdkReceives sdks)

-- ---------------------------------------------------------------------------
-- Derived facts
-- ---------------------------------------------------------------------------

-- | Everyone who receives data, in catalog order.
recipients :: Env -> [SdkEntry]
recipients = envSdks

-- | Vendors acting on our instructions.
processors :: Env -> [SdkEntry]
processors = filter ((== Processor) . sdkRole) . envSdks

-- | Vendors deciding their own purposes. Their own policies and contacts have
-- to be surfaced, because we cannot answer for them.
controllers :: Env -> [SdkEntry]
controllers = filter ((/= Processor) . sdkRole) . envSdks

-- | Vendors whose receipt of data is a sale or a share under the CCPA.
saleOrShareSdks :: Env -> [SdkEntry]
saleOrShareSdks = filter (sdkHasTrait SellsOrShares) . envSdks

transferSdks :: Env -> [SdkEntry]
transferSdks = filter (sdkHasTrait TransfersInternationally) . envSdks

-- | The distinct safeguards relied on across all recipients.
transferBases :: Env -> [TransferBasis]
transferBases = nub . concatMap sdkTransfers . envSdks

-- | Each collected category with the purposes it serves and the recipients
-- that receive it. This is the backbone of the collection disclosure and of
-- the CCPA notice at collection.
collectionTable :: Env -> [(DataCategory, [Purpose], [SdkEntry])]
collectionTable env =
  [ (cat, purposesFor cat, receiversOf cat)
  | cat <- Set.toList (envCollected env)
  ]
  where
    receiversOf cat =
      [ e | e <- envSdks env, cat `Set.member` sdkReceives e ]
    purposesFor cat =
      nub (concatMap (Set.toList . sdkPurposes) (receiversOf cat))

adFormats :: [Monetization]
adFormats = [BannerAds, InterstitialAds, RewardedAds]

iapFormats :: [Monetization]
iapFormats = [IapConsumable, IapNonConsumable, Subscription]

declaresAds :: Env -> Bool
declaresAds env = any (`elem` adFormats) (specMonetization (envSpec env))

declaresIap :: Env -> Bool
declaresIap env = any (`elem` iapFormats) (specMonetization (envSpec env))
