{-# LANGUAGE OverloadedStrings #-}

-- | Reading specs off disk, once, at startup.
--
-- Everything is loaded and validated before the server binds a port, and any
-- error aborts the process. Three things follow from that: a broken spec can
-- never reach a URL that an app store listing points at; there is no
-- filesystem access per request, so the old path-traversal hole is gone
-- structurally rather than by validation alone; and rendering is a pure
-- function of memory.
module Privgen.Load
  ( SpecLoadError (..)
  , LoadResult (..)
  , renderSpecLoadError
  , loadSpecs
  , loadSpecFile
  ) where

import           Data.List        (isSuffixOf, sort)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Yaml        (ParseException, decodeFileEither,
                                   prettyPrintParseException)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  (takeBaseName, (</>))

import           Privgen.Spec
import           Privgen.Types
import           Privgen.Validate

data SpecLoadError
  = SpecDirMissing FilePath
  | SpecParseFailed FilePath ParseException
  | SpecInvalid FilePath [SpecError]
  | SlugFilenameMismatch FilePath Slug
  | DuplicateSlug Slug FilePath FilePath

-- | Always lead with the file path. yaml only retains line and column for
-- syntax errors; an aeson-level failure arrives as a bare JSONPath, which is
-- useless to whoever is staring at a failed deploy.
renderSpecLoadError :: SpecLoadError -> Text
renderSpecLoadError (SpecDirMissing dir) =
  "spec directory does not exist: " <> T.pack dir
renderSpecLoadError (SpecParseFailed path e) =
  T.pack path <> ": " <> T.pack (prettyPrintParseException e)
renderSpecLoadError (SpecInvalid path es) =
  T.intercalate "\n" [ T.pack path <> ": " <> renderSpecError e | e <- es ]
renderSpecLoadError (SlugFilenameMismatch path s) =
  T.pack path <> ": declares slug " <> unSlug s
    <> ", which does not match the filename. The filename is the URL, so the \
       \two must agree."
renderSpecLoadError (DuplicateSlug s a b) =
  "slug " <> unSlug s <> " is declared by both " <> T.pack a <> " and "
    <> T.pack b

-- | What a successful load produced.
data LoadResult = LoadResult
  { lrSpecs      :: Map Slug ValidSpec
  , lrAdvisories :: [Text]
    -- ^ Non-blocking findings, already formatted with their file path. The
    -- caller is expected to print these; they describe real compliance gaps
    -- that simply should not take a live policy URL offline.
  }

-- | Decode and validate one spec, checking it against its filename.
loadSpecFile :: FilePath -> IO (Either SpecLoadError (ValidSpec, [Text]))
loadSpecFile path = do
  parsed <- decodeFileEither path
  pure $ case parsed of
    Left e     -> Left (SpecParseFailed path e)
    Right spec ->
      case validateSpec spec of
        Left es -> Left (SpecInvalid path es)
        Right (v, advisories)
          | unSlug (specSlug spec) /= T.pack (takeBaseName path) ->
              Left (SlugFilenameMismatch path (specSlug spec))
          | otherwise ->
              Right (v, [ T.pack path <> ": " <> renderSpecError a
                        | a <- advisories ])

-- | Load every @*.yaml@ in a directory. Reports every failure, not the first.
loadSpecs :: FilePath -> IO (Either [SpecLoadError] LoadResult)
loadSpecs dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Left [SpecDirMissing dir])
    else do
      names <- listDirectory dir
      let paths = sort [ dir </> n | n <- names, isYaml n ]
      results <- mapM loadSpecFile paths
      pure (collect (zip paths results))
  where
    isYaml n = ".yaml" `isSuffixOf` n || ".yml" `isSuffixOf` n

collect
  :: [(FilePath, Either SpecLoadError (ValidSpec, [Text]))]
  -> Either [SpecLoadError] LoadResult
collect = go [] [] [] Map.empty
  where
    go :: [SpecLoadError]        -- ^ errors so far, reversed
       -> [Text]                 -- ^ advisories so far, reversed
       -> [(Slug, FilePath)]     -- ^ which file claimed each slug
       -> Map Slug ValidSpec
       -> [(FilePath, Either SpecLoadError (ValidSpec, [Text]))]
       -> Either [SpecLoadError] LoadResult
    go errs advs _ acc [] =
      if null errs
        then Right (LoadResult acc (reverse advs))
        else Left (reverse errs)
    go errs advs seen acc ((path, res) : rest) =
      case res of
        Left e -> go (e : errs) advs seen acc rest
        Right (v, vAdvs) ->
          let s = specSlug (unValid v)
          in case lookup s seen of
               Just prev ->
                 go (DuplicateSlug s prev path : errs) advs seen acc rest
               Nothing ->
                 go errs (reverse vAdvs <> advs) ((s, path) : seen)
                    (Map.insert s v acc) rest
