{-# LANGUAGE OverloadedStrings #-}

-- | Turning a selected document into HTML.
--
-- House rule: blaze is imported qualified everywhere in this codebase, as @H@
-- and @A@. "Text.Blaze.Html5" exports @head@, @map@, @div@, @id@, @span@,
-- @title@ and @form@; its @Attributes@ module exports @id@, @class_@, @form@,
-- @label@, @span@ and @style@. The previous version of this file contorted
-- around those collisions with a hand-picked import list. With a corpus of
-- this size, every module has to do it the same way.
module Privgen.Render
  ( renderDoc
  , renderDocText
  , renderAudit
  , renderNotFound
    -- * Helpers for corpus modules
  , p_
  , ul_
  , link_
  , email_
  , defList_
  , formatDay
  ) where

import qualified Data.List.NonEmpty            as NE
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Time.Calendar            (Day)
import           Data.Time.Format              (defaultTimeLocale, formatTime)
import           Text.Blaze.Html               (Html, toHtml, toValue)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           Privgen.Condition
import           Privgen.Document
import           Privgen.Env
import           Privgen.Spec
import           Privgen.Types

-- ---------------------------------------------------------------------------
-- Corpus helpers
-- ---------------------------------------------------------------------------

p_ :: Text -> Html
p_ = H.p . toHtml

ul_ :: [Html] -> Html
ul_ = H.ul . mapM_ H.li

link_ :: Text -> Text -> Html
link_ href label = H.a H.! A.href (toValue href) $ toHtml label

email_ :: Text -> Html
email_ addr = H.a H.! A.href (toValue ("mailto:" <> addr)) $ toHtml addr

-- | A term/description list, used for the collection and recipient tables.
defList_ :: [(Html, Html)] -> Html
defList_ rows = H.dl $ mapM_ row rows
  where
    row (term, desc) = H.dt term >> H.dd desc

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%-d %B %Y"

-- ---------------------------------------------------------------------------
-- Document rendering
-- ---------------------------------------------------------------------------

docTitle :: SelectedDoc -> Text
docTitle sd = case docKind (sdDoc sd) of
  PrivacyDoc -> "Privacy Policy"
  TermsDoc   -> "Terms of Service"

-- | The sibling document, so each page links to the other. This is what turns
-- the old dangling "our Terms and Conditions, which is accessible at
-- <game name>" into a real link.
siblingLink :: SelectedDoc -> (Text, Text)
siblingLink sd = case docKind (sdDoc sd) of
  PrivacyDoc -> ("/terms/" <> slug, "Terms of Service")
  TermsDoc   -> ("/privacy/" <> slug, "Privacy Policy")
  where
    slug = unSlug (specSlug (envSpec (sdEnv sd)))

renderDoc :: SelectedDoc -> Html
renderDoc sd = H.docTypeHtml H.! A.lang "en" $ do
  H.head $ do
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.name "viewport"
           H.! A.content "width=device-width, initial-scale=1"
    H.title (toHtml (name <> " \8212 " <> docTitle sd))
    H.link H.! A.rel "stylesheet" H.! A.href "/static/privacy.css"
  H.body $ H.div H.! A.class_ "document" $ do
    H.h1 (toHtml (docTitle sd))
    H.p H.! A.class_ "subtitle" $ do
      H.strong (toHtml name)
      toHtml (" \183 Last updated " <> formatDay (specUpdated spec))
    tableOfContents sd
    mapM_ (renderSection env) (sdSections sd)
    H.hr
    H.p H.! A.class_ "sibling" $ do
      "See also our "
      uncurry link_ (siblingLink sd)
      "."
  where
    env  = sdEnv sd
    spec = envSpec env
    name = specName spec

tableOfContents :: SelectedDoc -> Html
tableOfContents sd =
  H.nav H.! A.class_ "toc" $ ul_ (map entry (sdSections sd))
  where
    entry ss =
      let sec = ssSection ss
      in case secAnchor sec of
           Just (Anchor a) -> link_ ("#" <> a) (secHeading sec)
           Nothing         -> toHtml (secHeading sec)

renderSection :: Env -> SelectedSection -> Html
renderSection env ss = H.section $ do
  case secAnchor sec of
    Just (Anchor a) -> H.h2 H.! A.id (toValue a) $ toHtml (secHeading sec)
    Nothing         -> H.h2 (toHtml (secHeading sec))
  mapM_ (\sc -> clBody (scClause sc) env) (NE.toList (ssClauses ss))
  where
    sec = ssSection ss

renderDocText :: SelectedDoc -> Text
renderDocText = LT.toStrict . renderHtml . renderDoc

-- ---------------------------------------------------------------------------
-- Audit
-- ---------------------------------------------------------------------------

-- | Which clauses were selected, why, and what each one is there to satisfy.
--
-- This, not the HTML, is what a reviewer should read. Committed as a golden
-- file, it turns every wording or selection change into a reviewable diff on a
-- file named after the game.
renderAudit :: SelectedDoc -> Text
renderAudit sd = T.unlines $
  [ "document: " <> docTitle sd
  , "game:     " <> specName spec
  , "slug:     " <> unSlug (specSlug spec)
  , "corpus:   " <> corpusVersion
  , "updated:  " <> formatDay (specUpdated spec)
  , "regions:  " <> T.intercalate ", " (map regionLabel (specRegions spec))
  , "audience: " <> audienceLabel (specAudience spec)
  , ""
  ] <> concatMap sectionLines (sdSections sd)
  where
    spec = envSpec (sdEnv sd)

    sectionLines ss =
      let SectionId sid = secId (ssSection ss)
      in ("## " <> sid)
           : concatMap clauseLines (NE.toList (ssClauses ss))
           <> [""]

    clauseLines sc =
      let c            = scClause sc
          ClauseId cid = clId c
      in [ "  " <> cid ]
           <> [ "    cites: " <> T.intercalate "; " (clCites c)
              | not (null (clCites c)) ]
           <> map ("    " <>) (renderTrace (scTrace sc))

-- ---------------------------------------------------------------------------

-- | Served with a 404 status. The previous implementation returned this page
-- with a 200, which reads to a store crawler as a live policy URL.
renderNotFound :: Slug -> Html
renderNotFound s = H.docTypeHtml H.! A.lang "en" $ do
  H.head $ do
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.name "viewport"
           H.! A.content "width=device-width, initial-scale=1"
    H.title "Not found"
    H.link H.! A.rel "stylesheet" H.! A.href "/static/privacy.css"
  H.body $ H.div H.! A.class_ "document centred" $ do
    H.h1 "Not found"
    H.p $ do
      "No published game"
      H.code (toHtml (unSlug s))
      "exists, yet..."
