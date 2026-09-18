{-# LANGUAGE OverloadedStrings #-}

-- | Clause selection, asserted on clause ids rather than on rendered output.
--
-- This is the payoff of reifying conditions: these tests say exactly which
-- clauses a given spec pulls in, with no HTML anywhere near them.
module Privgen.SelectionSpec (spec) where

import           Test.Hspec

import           Privgen.Corpus.Privacy (privacyDocument)
import           Privgen.Corpus.Terms   (termsDocument)
import           Privgen.Document
import           Privgen.Fixtures
import           Privgen.Spec
import           Privgen.Types

privacyIds :: GameSpec -> [ClauseId]
privacyIds = selectedClauseIds . flip selectDoc privacyDocument . envFor

termsIds :: GameSpec -> [ClauseId]
termsIds = selectedClauseIds . flip selectDoc termsDocument . envFor

sectionIds :: GameSpec -> [SectionId]
sectionIds s =
  map (secId . ssSection) (sdSections (selectDoc (envFor s) privacyDocument))

spec :: Spec
spec = do
  describe "a European and US ad-supported game" $ do
    let ids = privacyIds euUsAdSupported

    it "states its GDPR legal bases" $
      ids `shouldContain` [ClauseId "privacy.legal-bases"]

    it "discloses sale and sharing under the CCPA" $
      ids `shouldContain` [ClauseId "privacy.rights.us.sale-share"]

    it "honours universal opt-out signals" $
      ids `shouldContain` [ClauseId "privacy.rights.us.gpc"]

    it "explains international transfers" $
      ids `shouldContain` [ClauseId "privacy.transfers"]

    it "does not include Turkish rights" $
      ids `shouldNotContain` [ClauseId "privacy.rights.kvkk"]

  describe "a US-only game with no advertising" $ do
    let ids = privacyIds usOnlyNoAds

    it "omits every GDPR clause" $
      ids `shouldNotContain` [ClauseId "privacy.legal-bases"]

    it "omits the sale and share disclosure, since nothing sells or shares" $
      ids `shouldNotContain` [ClauseId "privacy.rights.us.sale-share"]

    it "still carries US rights" $
      ids `shouldContain` [ClauseId "privacy.rights.us"]

    it "drops the advertising section entirely rather than leaving it empty" $
      sectionIds usOnlyNoAds `shouldNotContain` [SectionId "advertising"]

  describe "a child-directed game" $ do
    let ids = privacyIds childDirected

    it "carries the COPPA separate-consent clause" $
      ids `shouldContain` [ClauseId "privacy.children.consent"]

    it "states that advertising is contextual only" $
      ids `shouldContain` [ClauseId "privacy.children.no-behavioural"]

    it "carries a retention limit for children's data" $
      ids `shouldContain` [ClauseId "privacy.children.retention"]

    it "does not claim to be undirected to children" $
      ids `shouldNotContain` [ClauseId "privacy.children.general"]

  describe "a Turkish game" $ do
    let ids = privacyIds turkeyOnly

    it "carries KVKK rights" $
      ids `shouldContain` [ClauseId "privacy.rights.kvkk"]

    it "names its VERBIS registration" $
      ids `shouldContain` [ClauseId "privacy.verbis"]

    it "omits GDPR rights" $
      ids `shouldNotContain` [ClauseId "privacy.rights.gdpr"]

  describe "terms of service" $ do
    it "includes virtual items only when the game has them" $ do
      termsIds euUsAdSupported `shouldContain` [ClauseId "terms.virtual.licence"]
      termsIds usOnlyNoAds `shouldNotContain` [ClauseId "terms.virtual.licence"]

    it "includes the withdrawal right for European consumers" $ do
      termsIds euUsAdSupported `shouldContain` [ClauseId "terms.purchases.withdrawal"]

    it "omits arbitration unless it has been elected" $
      termsIds euUsAdSupported `shouldNotContain` [ClauseId "terms.law.arbitration"]

    it "includes arbitration when elected" $
      let elected = euUsAdSupported
            { specTerms = (specTerms euUsAdSupported) { tsArbitration = True } }
      in termsIds elected `shouldContain` [ClauseId "terms.law.arbitration"]

    it "carries Apple's required EULA terms on iOS" $
      termsIds euUsAdSupported `shouldContain` [ClauseId "terms.platform.apple"]

    it "omits Apple's terms for an Android-only release" $
      let androidOnly = euUsAdSupported { specPlatforms = [Android] }
      in termsIds androidOnly `shouldNotContain` [ClauseId "terms.platform.apple"]
