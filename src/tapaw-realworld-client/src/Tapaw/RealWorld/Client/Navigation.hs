{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapaw.RealWorld.Client.Navigation
  ( makeHistoryRouter
  , appLink
  , appLinkDyn
  ) where

import Control.Lens ((%~))
import Data.Proxy (Proxy(Proxy))
import Data.Map (Map)
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Window as DOM hiding (focus)
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
import Tapaw.RealWorld.Client.Types (Route, decodeRoute, encodeRoute)


makeHistoryRouter ::
     ( MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , PostBuild t m
     )
  => Route
  -> Event t Route
  -> m (Dynamic t Route)
makeHistoryRouter initialRoute eSetRoute = do
  window <- liftJSM DOM.currentWindowUnchecked
  location <- liftJSM (DOM.getLocation window)
  history <- liftJSM (DOM.getHistory window)
  iRoute <- liftJSM (getRoute location)
  pb <- getPostBuild
  eRoute <- wrapDomEvent window (`DOM.on` DOM.popState) (getRoute location)
  performEvent_ $ DOM.pushState history () ("" :: Text) . Just . encodeRoute <$> eSetRoute
  holdDyn initialRoute $ leftmost [iRoute <$ pb, eSetRoute, eRoute]
  where
    getRoute loc = (decodeRoute .) . (<>) <$> DOM.getPathname loc <*> DOM.getSearch loc

appLink ::
     (EventWriter t Route m, Reflex t, PostBuild t m, DomBuilder t m)
  => Route
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLink = appLinkDyn . pure

appLinkDyn ::
     forall t m.
     (EventWriter t Route m, Reflex t, PostBuild t m, DomBuilder t m)
  => Dynamic t Route
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLinkDyn dR dAttrs dDisabled inner = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $
    ffor2 dR dAttrs (\r attrs -> "href" =: encodeRoute r <> attrs)
  (e, ()) <- element "a" ((def :: ElementConfig EventResult t (DomBuilderSpace m))
    & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
    & elementConfig_eventSpec %~
        addEventSpecFlags
        (Proxy :: Proxy (DomBuilderSpace m))
        Click
        (const preventDefault)) inner
  tellEvent $ current dR <@ gate (current dDisabled) (domEvent Click e)
