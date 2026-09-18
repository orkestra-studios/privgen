{-# LANGUAGE OverloadedStrings #-}

-- | Golden files for the real committed game specs.
--
-- Two per document: the rendered HTML, and the audit view. The audit file is
-- the one a reviewer should read — it lists the clauses that fired and why,
-- so a wording or selection change arrives as a reviewable diff on a file
-- named after the game.
--
-- To capture or refresh them:
--
-- > PRIVGEN_GOLDEN_UPDATE=1 stack test
--
-- then read the diff before committing. Making that two steps rather than one
-- is deliberate: a golden you can update without looking at guards nothing.
module Privgen.GoldenSpec (spec) where

import           Control.Monad          (forM_)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist)
import           System.Environment     (lookupEnv)
import           System.FilePath        ((</>))
import           Test.Hspec

import           Privgen.Corpus.Privacy (privacyDocument)
import           Privgen.Corpus.Terms   (termsDocument)
import           Privgen.Document
import           Privgen.Env            (mkEnv)
import           Privgen.Fixtures       (fixedDay)
import           Privgen.Load
import           Privgen.Render
import           Privgen.Types          (unSlug)

goldenDir :: FilePath
goldenDir = "test/golden"

spec :: Spec
spec = do
  loaded <- runIO (loadSpecs "games")
  case loaded of
    Left errs ->
      it "loads the committed specs" $
        expectationFailure
          (T.unpack (T.intercalate "\n" (map renderSpecLoadError errs)))
    Right result ->
      forM_ (Map.toList (lrSpecs result)) $ \(slug, vs) -> do
        let name = T.unpack (unSlug slug)
            env  = mkEnv fixedDay vs
        describe name $
          forM_ [ ("privacy", privacyDocument)
                , ("terms",   termsDocument) ] $ \(kind, doc) -> do
            let sd = selectDoc env doc
            it (kind <> " html") $
              golden (goldenDir </> (name <> "." <> kind <> ".html"))
                     (renderDocText sd)
            it (kind <> " audit") $
              golden (goldenDir </> (name <> "." <> kind <> ".audit.txt"))
                     (renderAudit sd)

golden :: FilePath -> T.Text -> Expectation
golden path actual = do
  update <- lookupEnv "PRIVGEN_GOLDEN_UPDATE"
  exists <- doesFileExist path
  case (update, exists) of
    (Just _, _) -> do
      createDirectoryIfMissing True goldenDir
      TIO.writeFile path actual
    (Nothing, True) -> do
      expected <- TIO.readFile path
      actual `shouldBe` expected
    (Nothing, False) ->
      -- Pending rather than failing: the goldens have simply not been captured
      -- yet. Once they are committed this branch stops being reachable and the
      -- comparison above starts guarding every change.
      pendingWith
        (path <> " has not been captured yet; run \
                \PRIVGEN_GOLDEN_UPDATE=1 stack test and commit the result")
