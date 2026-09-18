{-# LANGUAGE OverloadedStrings #-}

-- | Entry point.
--
-- Specs are loaded, validated, and rendered once before the port is bound. If
-- anything fails, every failure is printed and the process exits non-zero, so
-- a broken spec or a corpus mistake fails the deploy rather than reaching a
-- reader. Rendering every document at startup costs milliseconds and catches
-- the class of bug that would otherwise surface as a 500 on a live policy URL.
module Main (main) where

import           Control.Monad          (forM_, unless)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (getCurrentTime, utctDay)
import           Network.Wai.Handler.Warp (run)
import           System.Environment     (lookupEnv)
import           System.Exit            (exitFailure)
import           System.IO              (BufferMode (LineBuffering),
                                         hSetBuffering, stderr, stdout)

import           Privgen.Corpus.Privacy (privacyDocument)
import           Privgen.Corpus.Terms   (termsDocument)
import           Privgen.Document
import           Privgen.Env            (mkEnv)
import           Privgen.Load
import           Privgen.Render         (renderDocText)
import           Privgen.Types          (unSlug)
import           Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  specDir <- envOr "PRIVGEN_SPECS" "games"
  port    <- readPort <$> envOr "PRIVGEN_PORT" "8080"
  audit   <- (== Just "1") <$> lookupEnv "PRIVGEN_AUDIT"
  today   <- utctDay <$> getCurrentTime

  loaded <- loadSpecs specDir
  result <- case loaded of
    Left errs -> die ("failed to load specs from " <> T.pack specDir)
                     (map renderSpecLoadError errs)
    Right r   -> pure r

  let specs = lrSpecs result

  -- Advisories are real compliance gaps that should not take a live policy
  -- URL offline. They are printed on every boot so they stay visible rather
  -- than becoming background noise in a file nobody opens.
  unless (null (lrAdvisories result)) $ do
    TIO.hPutStrLn stderr "privgen: advisory findings:"
    mapM_ (TIO.hPutStrLn stderr . ("  ! " <>)) (lrAdvisories result)

  -- The corpus itself is checked here rather than only in the test suite, so
  -- a duplicate id or a clause left conditioned on Never cannot ship.
  let lintErrors =
        concatMap corpusLint [privacyDocument, termsDocument]
  unless (null lintErrors) $
    die "corpus lint failed" (map renderLintError lintErrors)

  -- Render everything once. A clause that throws only for one particular spec
  -- should fail the deploy, not the reader.
  forM_ (Map.toList specs) $ \(slug, vs) -> do
    let env = mkEnv today vs
    forM_ [privacyDocument, termsDocument] $ \doc ->
      seqText (renderDocText (selectDoc env doc))
    TIO.putStrLn ("  ok  " <> unSlug slug)

  TIO.putStrLn
    (  "serving " <> T.pack (show (Map.size specs))
    <> " game(s) on port " <> T.pack (show port)
    <> (if audit then " (audit enabled)" else "") )

  run port (app (AppState specs today audit))

-- | Force the rendered document so a lazy failure surfaces here.
seqText :: T.Text -> IO ()
seqText t = T.length t `seq` pure ()

envOr :: String -> String -> IO String
envOr k fallback = fromMaybe fallback <$> lookupEnv k

readPort :: String -> Int
readPort s = case reads s of
  [(n, "")] -> n
  _         -> 8080

die :: T.Text -> [T.Text] -> IO a
die headline details = do
  TIO.hPutStrLn stderr ("privgen: " <> headline)
  mapM_ (TIO.hPutStrLn stderr . ("  " <>)) details
  exitFailure
