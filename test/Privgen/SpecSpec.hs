{-# LANGUAGE OverloadedStrings #-}

-- | Slug safety, decoding, and validation.
module Privgen.SpecSpec (spec) where

import           Data.Either      (isLeft, isRight)
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
import           Test.Hspec

import           Privgen.Catalog
import           Privgen.Fixtures
import           Privgen.Load
import           Privgen.Spec
import           Privgen.Types
import           Privgen.Validate

spec :: Spec
spec = do
  describe "slug parsing" $ do
    it "accepts the slugs already in production" $ do
      mkSlug "donut-rush-3d" `shouldSatisfy` isRight
      mkSlug "fruit-slice"   `shouldSatisfy` isRight

    it "rejects path traversal" $ do
      -- The old code concatenated the capture straight into a file path, so
      -- this is a security assertion, not a formatting one.
      mkSlug "../../etc/passwd" `shouldSatisfy` isLeft
      mkSlug ".."               `shouldSatisfy` isLeft
      mkSlug "a/b"              `shouldSatisfy` isLeft

    it "rejects anything outside the permitted alphabet" $ do
      mkSlug "Donut"      `shouldSatisfy` isLeft
      mkSlug "with space" `shouldSatisfy` isLeft
      mkSlug "under_score" `shouldSatisfy` isLeft
      mkSlug ""            `shouldSatisfy` isLeft
      mkSlug "-leading"    `shouldSatisfy` isLeft

    it "rejects an over-long slug" $
      mkSlug (T.replicate 65 "a") `shouldSatisfy` isLeft

  describe "validation" $ do
    it "accepts every fixture" $
      mapM_ (\s -> validateSpec s `shouldSatisfy` isRight)
        [ baseSpec, euUsAdSupported, usOnlyNoAds, childDirected, turkeyOnly ]

    it "refuses a child-directed game shipping a behavioural ad SDK" $
      let bad = childDirected
            { specSdks         = [MetaAudienceNetwork]
            , specMonetization = [BannerAds]
            }
      in validateSpec bad `shouldSatisfy` isLeft

    it "refuses advertising with no advertising SDK" $
      let bad = baseSpec { specMonetization = [BannerAds] }
      in validateSpec bad `shouldSatisfy` isLeft

    it "refuses a spec with no regions" $
      validateSpec (baseSpec { specRegions = [] }) `shouldSatisfy` isLeft

    it "refuses duplicate SDK entries" $
      let bad = baseSpec { specSdks = [GameAnalytics, GameAnalytics] }
      in validateSpec bad `shouldSatisfy` isLeft

    it "treats a missing Art. 27 representative as advisory, not blocking" $
      -- A paperwork gap in the business should not take a live policy URL
      -- offline, but it must be reported.
      let outsideEu = baseSpec
            { specRegions = [EEA]
            , specCompany = (specCompany baseSpec) { coCountry = "Turkiye" }
            }
      in case validateSpec outsideEu of
           Left es          -> expectationFailure ("blocked: " <> show es)
           Right (_, advis) -> advis `shouldContain` [MissingEuRepresentative]

  describe "loading the real spec directory" $
    it "loads every committed game spec" $ do
      result <- loadSpecs "games"
      case result of
        Left errs ->
          expectationFailure
            (T.unpack (T.intercalate "\n" (map renderSpecLoadError errs)))
        Right r -> Map.size (lrSpecs r) `shouldBe` 2
