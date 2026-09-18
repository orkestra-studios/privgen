{-# LANGUAGE OverloadedStrings #-}

-- | Whole-corpus invariants.
--
-- These are the tests that catch the failure mode that actually matters in a
-- compliance corpus: not a wrong clause, but a missing one.
module Privgen.CorpusSpec (spec) where

import           Data.List              (nub)
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import           Test.Hspec
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy         as LT

import           Privgen.Condition
import           Privgen.Corpus.Privacy (privacyDocument)
import           Privgen.Corpus.Terms   (termsDocument)
import           Privgen.Document
import           Privgen.Fixtures
import           Privgen.Render         (renderDocText)
import           Privgen.Spec
import           Privgen.Types

everyScenario :: [GameSpec]
everyScenario =
  [ baseSpec, euUsAdSupported, usOnlyNoAds, childDirected, turkeyOnly ]

spec :: Spec
spec = do
  describe "structural lint" $ do
    it "the privacy corpus is clean" $
      map renderLintError (corpusLint privacyDocument) `shouldBe` []

    it "the terms corpus is clean" $
      map renderLintError (corpusLint termsDocument) `shouldBe` []

  describe "the DataDeletion anchor" $
    -- Google Play deep-links this. It is the single highest-consequence thing
    -- in the codebase, so it gets an assertion across every scenario rather
    -- than one fixture.
    it "survives every scenario" $
      mapM_ (\s ->
        renderDocText (selectDoc (envFor s) privacyDocument)
          `shouldSatisfy` T.isInfixOf "id=\"DataDeletion\"")
        everyScenario

  describe "clause bodies" $
    -- The enforcement mechanism for the house rule in Privgen.Document: if a
    -- clause was selected, it must actually say something. This is what stops
    -- the old `partners _ = Html.p ""` bug from coming back.
    it "are never empty when the clause was selected" $
      mapM_ checkBodies everyScenario

  describe "vocabulary coverage" $ do
    it "every region with its own obligations is addressed by some clause" $
      -- RestOfWorld is deliberately excluded: it has no regime-specific
      -- disclosures of its own and is served by the unconditional clauses.
      -- Every other region must be named somewhere, or someone added a
      -- jurisdiction and never wrote its section.
      let mentioned = [ r | InRegion r <- mentionedAtoms privacyDocument ]
          needed    = filter (/= RestOfWorld) [minBound .. maxBound]
      in filter (`notElem` mentioned) needed `shouldBe` ([] :: [Region])

    it "every monetization type is mentioned across the two documents" $
      let atoms     = mentionedAtoms privacyDocument
                        <> mentionedAtoms termsDocument
          mentioned = [ m | Monetizes m <- atoms ]
          -- Ad and IAP formats are addressed collectively by DeclaresAds and
          -- DeclaresIap, so only the ones needing individual treatment must
          -- appear by name.
          needsOwnClause = [RewardedAds]
      in filter (`notElem` mentioned) needsOwnClause `shouldBe` []

  describe "selected documents" $
    it "always produce at least one section" $
      mapM_ (\s -> do
        length (sdSections (selectDoc (envFor s) privacyDocument))
          `shouldSatisfy` (> 0)
        length (sdSections (selectDoc (envFor s) termsDocument))
          `shouldSatisfy` (> 0))
        everyScenario

  describe "clause citations" $
    it "are free of duplicates within a clause" $
      mapM_ (\c -> nub (clCites c) `shouldBe` clCites c)
        (allClauses privacyDocument <> allClauses termsDocument)

checkBodies :: GameSpec -> Expectation
checkBodies s =
  mapM_ each
    [ sc
    | doc <- [privacyDocument, termsDocument]
    , ss  <- sdSections (selectDoc env doc)
    , sc  <- NE.toList (ssClauses ss)
    ]
  where
    env = envFor s

    each sc =
      let ClauseId cid = clId (scClause sc)
          rendered     = LT.toStrict (renderHtml (clBody (scClause sc) env))
      in if T.null (T.strip (stripTags rendered))
           then expectationFailure
                  (T.unpack ("clause " <> cid <> " rendered nothing"))
           else pure ()

-- | Crude tag stripper: enough to tell "a paragraph of text" from
-- "<p></p>", which is the only distinction this test needs.
stripTags :: T.Text -> T.Text
stripTags = T.concat . outside
  where
    outside t = case T.breakOn "<" t of
      (before, rest)
        | T.null rest -> [before]
        | otherwise   -> before : inside (T.drop 1 rest)

    inside t = case T.breakOn ">" t of
      (_, rest)
        | T.null rest -> []
        | otherwise   -> outside (T.drop 1 rest)
