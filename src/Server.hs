{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | Routes.
--
-- @\/privacy\/\<slug\>@ keeps the shape it has always had, because those URLs
-- are in shipped apps and store listings and cannot move. @\/terms\/\<slug\>@
-- is new.
--
-- Two behaviours differ from the previous implementation, and both matter: a
-- missing game now returns 404 rather than a 200 page that says "not found",
-- and a malformed slug is rejected by 'Privgen.Types.Slug''s
-- 'Web.HttpApiData.FromHttpApiData' instance before any handler runs.
module Server
  ( API
  , AppState (..)
  , api
  , app
  , server
  ) where

import           Control.Monad.Error.Class     (throwError)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Encoding       as LTE
import           Data.Time.Calendar            (Day)
import           Network.Wai                   (Application)
import           Servant
import           Servant.HTML.Blaze            (HTML)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           Privgen.Corpus.Privacy        (privacyDocument)
import           Privgen.Corpus.Terms          (termsDocument)
import           Privgen.Document
import           Privgen.Env                   (mkEnv)
import           Privgen.Render
import           Privgen.Types                 (Slug)
import           Privgen.Validate              (ValidSpec)

data AppState = AppState
  { appSpecs :: Map Slug ValidSpec
  , appToday :: Day
    -- ^ Frozen at boot. Rendering must not read the clock, or two requests a
    -- midnight apart would disagree and every golden file would rot.
  , appAudit :: Bool
    -- ^ Exposes @\/audit@. Off unless @PRIVGEN_AUDIT=1@: the audit view shows
    -- the whole selection trace, which is for reviewers, not readers.
  }

type API =
       "privacy" :> Capture "slug" Slug :> Get '[HTML] Html
  :<|> "terms"   :> Capture "slug" Slug :> Get '[HTML] Html
  :<|> "audit"   :> Capture "kind" Text :> Capture "slug" Slug
                 :> Get '[PlainText] Text
  :<|> "healthz" :> Get '[PlainText] Text
  :<|> "static"  :> Raw

api :: Proxy API
api = Proxy

app :: AppState -> Application
app = serve api . server

server :: AppState -> Server API
server st =
       renderOr404 st privacyDocument
  :<|> renderOr404 st termsDocument
  :<|> auditHandler st
  :<|> healthHandler st
  :<|> serveDirectoryWebApp "static"

lookupSpec :: AppState -> Slug -> Maybe ValidSpec
lookupSpec st s = Map.lookup s (appSpecs st)

renderOr404 :: AppState -> Document -> Slug -> Handler Html
renderOr404 st doc slug =
  case lookupSpec st slug of
    Nothing -> throwError (notFoundFor slug)
    Just vs -> pure (renderDoc (selectDoc (mkEnv (appToday st) vs) doc))

notFoundFor :: Slug -> ServerError
notFoundFor slug = err404
  { errBody    = LTE.encodeUtf8 (renderHtml (renderNotFound slug))
  , errHeaders = [("Content-Type", "text/html; charset=utf-8")]
  }

auditHandler :: AppState -> Text -> Slug -> Handler Text
auditHandler st kind slug
  | not (appAudit st) = throwError err404 { errBody = "audit is disabled" }
  | otherwise =
      case (docFor kind, lookupSpec st slug) of
        (Nothing, _) ->
          throwError err404 { errBody = "unknown document kind" }
        (_, Nothing) ->
          throwError err404 { errBody = "unknown game" }
        (Just doc, Just vs) ->
          pure (renderAudit (selectDoc (mkEnv (appToday st) vs) doc))
  where
    docFor "privacy" = Just privacyDocument
    docFor "terms"   = Just termsDocument
    docFor _         = Nothing

healthHandler :: AppState -> Handler Text
healthHandler st =
  pure (T.pack (show (Map.size (appSpecs st))) <> " documents loaded")
