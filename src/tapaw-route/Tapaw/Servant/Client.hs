{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Tapaw.Servant.Client
  ( serve
  , url
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.History (pushState)
import GHCJS.DOM.Location (getHref)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getHistory, getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import Reflex.Dom.Core
import Tapaw.Servant.AsApp (HasApp(..))
import Tapaw.Servant.Types (Err(..), Loc(..))
import URI.ByteString
  (Query(..), URI, URIRef(..), laxURIParserOptions, parseURI, serializeURIRef')

-- error page :: (api -> MkApp) -> Err -> m ()
-- /error?redirect=/admin/x

serve ::
     forall t m api.
     ( HasApp api
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , DomBuilder t m
     , PostBuild t m
     , MonadJSM (Performable m)
     , MonadJSM m
     )
  => Proxy api
  -> MkApp api (EventWriterT t Loc m)
  -> (Err -> EventWriterT t Loc m ())
  -> m ()
serve api ws showError = mdo
  dUrl <- url eUrl
  (_, eUrl) <- runEventWriterT $ dyn $ either showError id . (route api ws =<<) <$> dUrl
  pure ()

url ::
     ( MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , DomBuilder t m
     , MonadJSM (Performable m)
     , MonadJSM m
     )
  => Event t Loc
  -> m (Dynamic t (Either Err Loc))
url us = do
  window <- currentWindowUnchecked
  location <- getLocation window
  history <- getHistory window
  text2uri <$> getHref location >>= \case
    Left e -> do
      text ("Failed to parse URI, cannot continue: " <> T.pack (show e))
      pure . constDyn $ Left e
    Right u0 -> do
      performEvent_ $ pushState history () ("" :: Text) . Just . uri2text . loc2uri u0 <$> us
      ps <- wrapDomEvent window (`on` popState) $ fmap uri2loc . text2uri <$> getHref location
      holdDyn (Right $ uri2loc u0) (leftmost [ps, Right <$> us])
  where
    b2s = T.pack . BC.unpack . BC.fromStrict
    s2b = BC.toStrict . BC.pack . T.unpack
    text2uri = first Err500 . parseURI laxURIParserOptions . BC.toStrict . BC.pack . T.unpack
    uri2text = T.pack . BC.unpack . BC.fromStrict . serializeURIRef'
    uri2loc uri = Loc
      { locPath = drop 1 . T.splitOn "/" . b2s $ uriPath uri
      , locQuery = bimap b2s b2s <$> queryPairs (uriQuery uri)
      }
    loc2uri :: URI -> Loc -> URI
    loc2uri u0 loc = u0
      { uriPath = "/" <> s2b (T.intercalate "/" $ locPath loc)
      , uriQuery = Query $ bimap s2b s2b <$> locQuery loc
      , uriFragment = Nothing
      }
