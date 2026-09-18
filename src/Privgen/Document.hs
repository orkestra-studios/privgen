{-# LANGUAGE OverloadedStrings #-}

-- | Clauses, sections, and the selection pass.
--
-- Section order is list order. There are no priority numbers: they rot, and
-- nobody can review a diff of them. The list in a corpus module /is/ the table
-- of contents.
--
-- House rule for clause authors, and the reason the audit trail can be
-- trusted: __a clause body must not branch on whether it should appear__.
-- Every decision about inclusion belongs in 'clWhen'. If a clause has two
-- shapes, it is two clauses. Breaking this reproduces the old
-- @partners _ = Html.p ""@ bug at scale, and makes the audit file lie to
-- whoever is reviewing it — it would report a clause as included while the
-- page shows nothing. A property test enforces the total case.
module Privgen.Document
  ( -- * Corpus version
    corpusVersion
    -- * Identifiers
  , ClauseId (..)
  , SectionId (..)
  , Anchor (..)
  , DocKind (..)
    -- * Structure
  , Clause (..)
  , Section (..)
  , Document (..)
    -- * Construction
  , clause
  , citing
  , section
  , anchored
    -- * Selection
  , SelectedClause (..)
  , SelectedSection (..)
  , SelectedDoc (..)
  , selectDoc
  , selectedClauseIds
  , allClauses
    -- * Static analysis
  , LintError (..)
  , renderLintError
  , corpusLint
  , mentionedAtoms
  ) where

import           Data.List           (group, nub, sort)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import           Text.Blaze.Html     (Html)

import           Privgen.Condition
import           Privgen.Env

-- | Bumped by hand when clause wording changes.
--
-- A per-game @updated@ date alone will not do: if the corpus text moves, the
-- documents move with it and no game file changed. This is recorded in the
-- audit output so a reviewer can tell which corpus produced the text in front
-- of them.
corpusVersion :: Text
corpusVersion = "2.0.0"

newtype ClauseId = ClauseId Text
  deriving (Eq, Ord, Show)

newtype SectionId = SectionId Text
  deriving (Eq, Ord, Show)

-- | An HTML @id@. Some of these are load-bearing outside this codebase:
-- @DataDeletion@ is deep-linked by Google Play's data-deletion requirement.
newtype Anchor = Anchor Text
  deriving (Eq, Ord, Show)

data DocKind = PrivacyDoc | TermsDoc
  deriving (Eq, Ord, Show)

data Clause = Clause
  { clId    :: ClauseId
  , clWhen  :: Condition
  , clCites :: [Text]
    -- ^ The obligation this clause discharges, e.g. @"GDPR Art. 13(1)(f)"@.
    -- Carried into the audit file so a reviewer can see coverage directly.
  , clBody  :: Env -> Html
  }

data Section = Section
  { secId      :: SectionId
  , secHeading :: Text
  , secAnchor  :: Maybe Anchor
  , secWhen    :: Condition
  , secClauses :: [Clause]
  }

data Document = Document
  { docKind     :: DocKind
  , docSections :: [Section]
  }

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

clause :: Text -> Condition -> (Env -> Html) -> Clause
clause i w b = Clause (ClauseId i) w [] b

-- | Attach the legal basis a clause exists to satisfy.
citing :: Clause -> [Text] -> Clause
citing c cs = c { clCites = cs }

section :: Text -> Text -> [Clause] -> Section
section i heading cs = Section (SectionId i) heading Nothing Always cs

anchored :: Section -> Text -> Section
anchored s a = s { secAnchor = Just (Anchor a) }

-- ---------------------------------------------------------------------------
-- Selection
-- ---------------------------------------------------------------------------

data SelectedClause = SelectedClause
  { scClause :: Clause
  , scTrace  :: Trace
  }

-- | Note the 'NonEmpty'. A section with no applicable clauses is not
-- representable here, so the renderer cannot emit a bare heading or an empty
-- paragraph even if someone wanted it to.
data SelectedSection = SelectedSection
  { ssSection :: Section
  , ssClauses :: NonEmpty SelectedClause
  }

data SelectedDoc = SelectedDoc
  { sdDoc      :: Document
  , sdEnv      :: Env
  , sdSections :: [SelectedSection]
  }

selectDoc :: Env -> Document -> SelectedDoc
selectDoc env doc = SelectedDoc doc env (mapMaybe pick (docSections doc))
  where
    pick s
      | not (evalCondition env (secWhen s)) = Nothing
      | otherwise = SelectedSection s <$> NE.nonEmpty (chosen s)

    chosen s =
      [ SelectedClause c t
      | c <- secClauses s
      , let t = explain env (clWhen c)
      , trResult t
      ]

selectedClauseIds :: SelectedDoc -> [ClauseId]
selectedClauseIds sd =
  [ clId (scClause sc)
  | ss <- sdSections sd
  , sc <- NE.toList (ssClauses ss)
  ]

allClauses :: Document -> [Clause]
allClauses = concatMap secClauses . docSections

-- ---------------------------------------------------------------------------
-- Static analysis
-- ---------------------------------------------------------------------------

data LintError
  = DuplicateClauseId ClauseId
  | DuplicateSectionId SectionId
  | DuplicateAnchor Anchor
  | StagedClause ClauseId
  | SectionWithNoClauses SectionId
  deriving (Eq, Show)

renderLintError :: LintError -> Text
renderLintError (DuplicateClauseId (ClauseId i)) =
  "duplicate clause id: " <> i
renderLintError (DuplicateSectionId (SectionId i)) =
  "duplicate section id: " <> i
renderLintError (DuplicateAnchor (Anchor a)) =
  "duplicate anchor: " <> a
renderLintError (StagedClause (ClauseId i)) =
  "clause " <> i <> " is conditioned on Never; a staged draft was left in"
renderLintError (SectionWithNoClauses (SectionId i)) =
  "section " <> i <> " declares no clauses at all"

-- | Structural checks over a whole document. Run in the test suite and again
-- at boot, so a corpus mistake fails the deploy rather than reaching a reader.
--
-- Clause ids are checked per document, not globally: shared clauses are
-- ordinary values and legitimately appear in both documents.
corpusLint :: Document -> [LintError]
corpusLint doc = concat
  [ map DuplicateClauseId  (duplicates (map clId (allClauses doc)))
  , map DuplicateSectionId (duplicates (map secId (docSections doc)))
  , map DuplicateAnchor    (duplicates (mapMaybe secAnchor (docSections doc)))
  , [ StagedClause (clId c) | c <- allClauses doc, clWhen c == Never ]
  , [ SectionWithNoClauses (secId s)
    | s <- docSections doc, null (secClauses s) ]
  ]

duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe pick . group . sort
  where
    pick (x : _ : _) = Just x
    pick _           = Nothing

-- | Every condition atom the document mentions anywhere.
--
-- Tests use this to catch the failure that matters most in a compliance
-- corpus: not a wrong clause, but a missing one. If a 'Region' or a
-- 'DataCategory' appears in the vocabulary and no clause anywhere mentions it,
-- somebody added a dimension and never wrote its disclosure.
mentionedAtoms :: Document -> [Condition]
mentionedAtoms doc = nub $ concatMap conditionAtoms $
  map clWhen (allClauses doc) <> map secWhen (docSections doc)
