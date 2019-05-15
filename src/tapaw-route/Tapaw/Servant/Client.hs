{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Tapaw.Servant.Client
  ( getInitialRouteHistory
  , getInitialRouteHash
  , runRoutedTFrozen
  , runRoutedTHistory
  , runRoutedTHash
  , appLink
  , appLink'
  , appLinkDyn
  , serve
  , url
  ) where

import Control.Lens ((%~))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.History (pushState)
import GHCJS.DOM.Location (getHref, getHash, setHash)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getHistory, getLocation)
import GHCJS.DOM.WindowEventHandlers (popState, hashChange)
import Reflex.Dom.Core
import Servant.API (IsElem)
import Servant.API.Generic (AsApi, GenericServant, ToServantApi)
import Tapaw.Servant.AsApp (HasApp(..))
import Tapaw.Servant.AsAppLink (GatherLinkArgs, HasAppLink)
import Tapaw.Servant.Routed (MonadRouted(..), RoutedT(..), SomeRoute(..), someRouteToLoc)
import Tapaw.Servant.TupleProduct (TupleProductOf)
import Tapaw.Servant.Types (Err(..), Loc(..))
import URI.ByteString
  (Query(..), URI, URIRef(..), laxURIParserOptions, parseURI, parseRelativeRef, serializeURIRef')

-- error page :: (api -> MkApp) -> Err -> m ()
-- /error?redirect=/admin/x

appLink ::
     ( GenericServant r AsApi
     , IsElem e (ToServantApi r)
     , HasAppLink e
     , MonadRouted r t m
     , PostBuild t m
     , DomBuilder t m
     )
  => (r AsApi -> e)
  -> TupleProductOf (GatherLinkArgs e)
  -> m ()
  -> m ()
appLink r a = appLinkDyn (pure (SomeRoute r a)) (pure M.empty) (pure True)

appLink' ::
     ( GenericServant r AsApi
     , IsElem e (ToServantApi r)
     , HasAppLink e
     , MonadRouted r t m
     , PostBuild t m
     , DomBuilder t m
     )
  => (r AsApi -> e)
  -> TupleProductOf (GatherLinkArgs e)
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLink' r a = appLinkDyn (constDyn (SomeRoute r a))

appLinkDyn ::
     forall t r m.
     (GenericServant r AsApi, MonadRouted r t m, PostBuild t m, DomBuilder t m)
  => Dynamic t (SomeRoute r)
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLinkDyn dR dAttrs dDisabled inner = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $
    ffor2 dR dAttrs (\r attrs -> "href" =: locToHash (someRouteToLoc r) <> attrs)
  (e, ()) <- element "a" ((def :: ElementConfig EventResult t (DomBuilderSpace m))
    & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
    & elementConfig_eventSpec %~
        addEventSpecFlags
        (Proxy :: Proxy (DomBuilderSpace m))
        Click
        (const preventDefault)) inner
  setRoute $ current dR <@ gate (current dDisabled) (domEvent Click e)

runRoutedTFrozen :: PerformEvent t m => Loc -> RoutedT t r m a -> m a
runRoutedTFrozen loc f = do
  (a, _) <- runEventWriterT $ runReaderT (unRoutedT f) (pure $ Right loc)
  pure a

runRoutedTHistory ::
     ( MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => (URI, Loc)
  -> RoutedT t r m a
  -> m a
runRoutedTHistory (uri0, loc0) f = do
  window <- currentWindowUnchecked
  location <- getLocation window
  history <- getHistory window
  rec
    performEvent_ $ pushState history () ("" :: Text) . Just . uriToText . locToUri uri0 <$> eUrl
    ps <- wrapDomEvent window (`on` popState) $ fmap uriToLoc . textToUri <$> getHref location
    loc <- holdDyn (Right loc0) (leftmost [ps, Right <$> eUrl])
    (a, eUrl) <- runEventWriterT $ runReaderT (unRoutedT f) loc
  pure a

runRoutedTHash ::
     ( MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Loc
  -> RoutedT t r m a
  -> m a
runRoutedTHash loc0 f = do
  window <- currentWindowUnchecked
  location <- getLocation window
  rec
    performEvent_ $ setHash location . locToHash <$> eUrl
    ps <- wrapDomEvent window (`on` hashChange) (hashToLoc <$> getHash location)
    loc <- holdDyn (Right loc0) (leftmost [ps, Right <$> eUrl])
    (a, eUrl) <- runEventWriterT $ runReaderT (unRoutedT f) loc
  pure a

getInitialRouteHistory :: MonadJSM m => m (Either Err (URI, Loc))
getInitialRouteHistory = do
  href <- getHref =<< getLocation =<< currentWindowUnchecked
  pure $ (\x -> (x, uriToLoc x)) <$> textToUri href

getInitialRouteHash :: MonadJSM m => m (Either Err Loc)
getInitialRouteHash = do
  href <- getHref =<< getLocation =<< currentWindowUnchecked
  pure $ hashToLoc href


hashToLoc :: Text -> Either Err Loc
hashToLoc x = case parseRelativeRef laxURIParserOptions (pre x) of
  Left e -> Left (Err500 e)
  Right uri -> Right $ Loc
    { locPath = drop 1 . T.splitOn "/" . b2s $ rrPath uri
    , locQuery = bimap b2s b2s <$> queryPairs (rrQuery uri)
    }
  where
    pre = s2b . T.dropWhile (== '!')

locToHash :: Loc -> Text
locToHash loc = b2s . ("!" <>) . serializeURIRef' $ RelativeRef
  { rrAuthority = Nothing
  , rrFragment = Nothing
  , rrPath = "/" <> s2b (T.intercalate "/" $ locPath loc)
  , rrQuery = Query $ bimap s2b s2b <$> locQuery loc
  }

b2s :: ByteString -> Text
b2s = T.pack . BC.unpack . BC.fromStrict

s2b :: Text -> ByteString
s2b = BC.toStrict . BC.pack . T.unpack

textToUri :: Text -> Either Err URI
textToUri = first Err500 . parseURI laxURIParserOptions . BC.toStrict . BC.pack . T.unpack

uriToText :: URI -> Text
uriToText = T.pack . BC.unpack . BC.fromStrict . serializeURIRef'

uriToLoc :: URI -> Loc
uriToLoc uri = Loc
  { locPath = drop 1 . T.splitOn "/" . b2s $ uriPath uri
  , locQuery = bimap b2s b2s <$> queryPairs (uriQuery uri)
  }

locToUri :: URI -> Loc -> URI
locToUri u0 loc = u0
  { uriPath = "/" <> s2b (T.intercalate "/" $ locPath loc)
  , uriQuery = Query $ bimap s2b s2b <$> locQuery loc
  , uriFragment = Nothing
  }

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
  -> MkApp api (EventWriterT t Loc m ())
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
  textToUri <$> getHref location >>= \case
    Left e -> do
      text ("Failed to parse URI, cannot continue: " <> T.pack (show e))
      pure . constDyn $ Left e
    Right u0 -> do
      performEvent_ $ pushState history () ("" :: Text) . Just . uriToText . locToUri u0 <$> us
      ps <- wrapDomEvent window (`on` popState) $ fmap uriToLoc . textToUri <$> getHref location
      holdDyn (Right $ uriToLoc u0) (leftmost [ps, Right <$> us])
