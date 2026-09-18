{-# LANGUAGE OverloadedStrings #-}

-- | Clauses that appear in both documents.
--
-- Sharing is list membership: a clause is an ordinary value, so both corpora
-- can name the same one. This is why 'Privgen.Document.corpusLint' checks
-- clause-id uniqueness per document rather than globally.
module Privgen.Corpus.Common
  ( companyIdentity
  , contactClause
  , changesClause
  , disclaimerNote
  , addressLines
  ) where

import           Data.Text                   (Text)
import           Text.Blaze.Html             (Html, toHtml)
import qualified Text.Blaze.Html5            as H

import           Privgen.Condition
import           Privgen.Document
import           Privgen.Env
import           Privgen.Render
import           Privgen.Spec

addressLines :: [Text] -> Html
addressLines [] = pure ()
addressLines ls = H.address $ sequence_
  [ toHtml l >> H.br | l <- ls ]

-- | Who the reader is dealing with. GDPR Art. 13(1)(a)-(b) and KVKK m.10 both
-- require the controller to be identified by name, not by brand.
companyIdentity :: Clause
companyIdentity =
  clause "common.company-identity" Always (\env ->
    let co = specCompany (envSpec env)
    in do
      H.p $ do
        "This document is published by "
        H.strong (toHtml (coLegalName co))
        ", the controller responsible for the personal data described here."
      addressLines (coAddress co <> [coCountry co])
      H.p $ do
        "You can reach our privacy team at "
        email_ (coPrivacyEmail co)
        "."
  ) `citing` ["GDPR Art. 13(1)(a)", "GDPR Art. 13(1)(b)", "KVKK m.10"]

-- | Notice of changes. GDPR expects changes to be communicated; app stores
-- expect a dated document.
changesClause :: Clause
changesClause =
  clause "common.changes" Always (\env -> do
    p_ "We update this document when the game changes, when the services we \
       \rely on change, or when the law does. The date at the top of this \
       \page always reflects the version you are reading."
    H.p $ do
      "Where a change materially affects your rights, we will give notice in \
       \the game before it takes effect \8212 we will not rely on your \
       \noticing that this page has moved. Earlier versions are available on \
       \request from "
      email_ (coPrivacyEmail (specCompany (envSpec env)))
      "."
  ) `citing` ["GDPR Art. 12(1)"]

contactClause :: Clause
contactClause =
  clause "common.contact" Always (\env ->
    let co = specCompany (envSpec env)
    in do
      H.p "If you have a question, a complaint, or a request about your data, \
          \write to us. A person reads these."
      defList_ $
        [ ("Privacy and data protection", email_ (coPrivacyEmail co))
        , ("Everything else", email_ (coSupportEmail co))
        ]
        <> [ ("Website", link_ w w) | Just w <- [coWebsite co] ]
  ) `citing` ["GDPR Art. 13(1)(b)"]

-- | An honest note about what this document is. Generated text is a starting
-- point that has been reviewed, not a substitute for the review.
disclaimerNote :: Clause
disclaimerNote =
  clause "common.scope-note" Always (\env ->
    p_ ("This document covers " <> specName (envSpec env)
        <> " only. Other games and services we publish have their own \
           \documents, and the practices described here do not apply to them.")
  )
