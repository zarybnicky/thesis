{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Common.PopState
  ( url
  ) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.History (pushState)
import GHCJS.DOM.Location (getHref)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getHistory, getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import Reflex.Class (MonadHold(..), Reflex(..), ffor, leftmost)
import Reflex.Dom.Builder.Immediate (wrapDomEventMaybe)
import Reflex.Dynamic (attachPromptlyDyn)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.TriggerEvent.Class (TriggerEvent)
import Servant.Common.Uri (Authority(..), Uri(..), encodeUrl, parseUrl)

url ::
     ( MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadJSM m
     )
  => Dynamic t Authority
  -> Event t Uri
  -> m (Dynamic t Uri)
url dAuthority us = do
  window <- currentWindowUnchecked
  loc <- getLocation window
  history <- getHistory window
  loc0 <- getHref loc
  let u0 = snd . either (error . ("No parse of window location because: " ++)) id $ parseOnly parseUrl loc0
  performEvent_ . ffor (attachPromptlyDyn dAuthority us) $
    pushState history () ("" :: Text) . Just . uncurry encodeUrl
  ps <- wrapDomEventMaybe window (`on` popState) $
    either (const Nothing) (Just . snd) . parseOnly parseUrl <$> getHref loc
  holdDyn u0 $ leftmost [ps, us]
