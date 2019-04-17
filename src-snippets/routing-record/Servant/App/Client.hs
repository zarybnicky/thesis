{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.App.Client
  ( serve
  , url
  ) where

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
import Servant.App.AsApp (HasApp(..))
import Servant.App.Types (Err(..), Loc(..), Context)
import URI.ByteString
  (Query(..), URI, URIRef(..), laxURIParserOptions, parseURI, serializeURIRef')

-- error page :: (api -> AppT) -> Err -> m ()
-- Think about /error?redirect=/admin/x

serve ::
     forall t m api. (HasApp api (), MonadWidget t m)
  => Proxy api
  -> AppT api t (EventWriterT t Loc m)
  -> (Err -> EventWriterT t Loc m ())
  -> m ()
serve api ws showError = mdo
  let ctx = undefined :: Context () t
  dUrl <- url eUrl
  (_, eUrl) <- runEventWriterT $ dyn $ either showError id . (route api ctx ws =<<) <$> dUrl
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
    text2uri = first Err500 . parseURI laxURIParserOptions . BC.toStrict . BC.pack . T.unpack
    uri2text = T.pack . BC.unpack . BC.fromStrict . serializeURIRef'
    uri2loc uri = Loc
      { locPath = drop 1 . BC.split '/' . BC.fromStrict $ uriPath uri
      , locQuery = bimap BC.fromStrict BC.fromStrict <$> queryPairs (uriQuery uri)
      }
    loc2uri :: URI -> Loc -> URI
    loc2uri u0 loc = u0
      { uriPath = "/" <> BC.toStrict (BC.intercalate "/" (locPath loc))
      , uriQuery = Query $ bimap BC.toStrict BC.toStrict <$> locQuery loc
      , uriFragment = Nothing
      }
