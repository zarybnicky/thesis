{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Tapaw.ServiceWorker.Client
  ( MonadServiceWorker(..)
  , ServiceWorkerT(..)
  , ServiceWorkerState(..)
  , runServiceWorkerT

  , hasServiceWorkerSupport
  , registerServiceWorker
  , fetchPushSubscription
  , fetchPushPermissionState
  , showNotificationImpl
  , onNotificationImpl
  , vapidKeyToArray

  , module Tapaw.ServiceWorker.Client.Types
  ) where

import Control.Lens ((^.), itraverse_)
import Control.Monad.Reader
import Data.Aeson (FromJSON, Result(Success), Value(Null), fromJSON, toJSON)
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle hiding (Success)
import Reflex.Dom.Core hiding (Value)
import Tapaw.ServiceWorker.Client.Types

hasServiceWorkerSupport :: MonadJSM m => m Bool
hasServiceWorkerSupport = liftJSM $ do
  a <- ghcjsPure . isTruthy =<< jsg ("navigator" :: Text) ! ("serviceWorker" :: Text)
  b <- ghcjsPure . isTruthy =<< jsg ("window" :: Text) ! ("PushManager" :: Text)
  pure (a && b)

registerServiceWorker ::
     (TriggerEvent t m, MonadJSM m)
  => Text
  -> ServiceWorkerOptions
  -> m (Event t ServiceWorkerRegistration)
registerServiceWorker swUrl swOpts = do
  (eReg, onReg) <- newTriggerEvent
  _ <- liftJSM $ do
    serviceWorker <- jsg ("navigator" :: Text) ^. js ("serviceWorker" :: Text)
    res <- serviceWorker ^. jsf ("register" :: Text) [toJSON swUrl, toJSON swOpts]
    res ^. jsf ("then" :: Text)
      [fun $ \_ _ [swReg] -> liftIO $ onReg (ServiceWorkerRegistration swReg)]
  pure eReg

fetchPushSubscription ::
     (MonadJSM m)
  => (Maybe PushSubscription -> IO ())
  -> ServiceWorkerRegistration
  -> m ()
fetchPushSubscription cb reg = liftJSM $ do
  mgr <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text)
  res <- mgr ^. js0 ("getSubscription" :: Text)
  _ <- res ^. jsf ("then" :: Text) [fun $ \_ _ [sub] -> do
    sub' <- maybeNullOrUndefined sub
    liftIO $ cb (PushSubscription <$> sub')]
  pure ()

fetchPushPermissionState ::
     MonadJSM m
  => (PermissionState -> IO ())
  -> ServiceWorkerRegistration
  -> m ()
fetchPushPermissionState cb reg = liftJSM $ do
  mgr <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text)
  res <- mgr ^. js1 ("permissionState" :: Text) (PushSubscriptionOptions True Nothing)
  _ <- res ^. jsf ("then" :: Text) [fun $ \_ _ [perm] -> do
    perm' <- fromJSVal perm
    liftIO $ cb (maybe PermissionDenied (fromSuccess PermissionDenied . fromJSON) perm')]
  pure ()
  where
    fromSuccess d = \case
      Success a -> a
      _ -> d

data ServiceWorkerState t = ServiceWorkerState
  { swStateRegistration :: Dynamic t (Maybe ServiceWorkerRegistration)
  , swStateSubscription :: Dynamic t (Maybe PushSubscription)
  , swStatePushPermission :: Dynamic t PermissionState
  }

newtype ServiceWorkerT t n m a = ServiceWorkerT
  { unServiceWorkerT :: ReaderT (ServiceWorkerState t) (EventWriterT t (Maybe PushSubscriptionOptions) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadIO
#ifndef ghcjs_HOST_OS
             , MonadJSM
#endif
             , MonadSample t
             , MonadHold t
             , PostBuild t
             , NotReady t
             , TriggerEvent t
             , DomBuilder t
             , Prerender js t
             )

instance MonadTrans (ServiceWorkerT t n) where
  lift = ServiceWorkerT . lift . lift

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (ServiceWorkerT t n m) where
  runWithReplace a b = ServiceWorkerT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = ServiceWorkerT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = ServiceWorkerT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance PerformEvent t m => PerformEvent t (ServiceWorkerT t n m) where
  type Performable (ServiceWorkerT t n m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

runServiceWorkerT ::
     ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     )
  => Text
  -> ServiceWorkerOptions
  -> proxy n
  -> ServiceWorkerT t n m a
  -> m a
runServiceWorkerT swUrl swOpts _ f = do
  (eSub, onSub) <- newTriggerEvent
  (ePerm, onPerm) <- newTriggerEvent
  eReg <- registerServiceWorker swUrl swOpts
  performEvent_ (fetchPushSubscription onSub <$> eReg)
  performEvent_ (fetchPushPermissionState onPerm <$> eReg)
  dReg <- holdDyn Nothing (Just <$> eReg)
  rec
    dSub <- holdDyn Nothing eSub
    dPerm <- holdDyn PermissionDenied ePerm
    (a, eSubOrUnsub) <- runEventWriterT $ runReaderT (unServiceWorkerT f)
      (ServiceWorkerState dReg dSub dPerm)
    performEvent_ (subOrUnsub onSub onPerm <$> current dReg <*> current dSub <@> eSubOrUnsub)
  pure a
  where
    subOrUnsub onSub onPerm mReg mSub mSubOpts = case (mReg, mSub, mSubOpts) of
      (Just reg, Just sub, Nothing) -> liftJSM $ do
        res <- unPushSubscription sub ^. js0 ("unsubscribe" :: Text)
        refreshSubPerm reg onSub onPerm res
      (Just reg, _, Just subOpts) -> liftJSM $ do
        res <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text) . js1 ("subscribe" :: Text) subOpts
        refreshSubPerm reg onSub onPerm res
      _ -> pure ()
    refreshSubPerm reg onSub onPerm res = do
      _ <- res ^. jsf ("then" :: Text) [fun $ \_ _ [_] -> fetchPushSubscription onSub reg]
      _ <- res ^. jsf ("then" :: Text) [fun $ \_ _ [_] -> fetchPushPermissionState onPerm reg]
      pure ()

class MonadServiceWorker t a m | m -> a t where
  getSWRegistration :: m (Dynamic t (Maybe ServiceWorkerRegistration))
  getPushSubscription :: m (Dynamic t (Maybe PushSubscription))
  getPushPermissionState :: m (Dynamic t PermissionState)
  subscribe :: Event t PushSubscriptionOptions -> m () -- tellEvent (Maybe PushSubscriptionOptions)
  unsubscribe :: Event t () -> m ()
  showNotification :: Event t (Text, Maybe NotificationOptions) -> m ()
  onNotification :: m (Event t a)

instance ( Monad m
         , Reflex t
         , PerformEvent t m
         , TriggerEvent t m
         , MonadJSM m
         , MonadJSM (Performable m)
         , FromJSON n
         ) =>
         MonadServiceWorker t n (ServiceWorkerT t n m) where
  getSWRegistration = ServiceWorkerT $ asks (\(ServiceWorkerState x _ _) -> x)
  getPushSubscription = ServiceWorkerT $ asks (\(ServiceWorkerState _ x _) -> x)
  getPushPermissionState =
    ServiceWorkerT $ asks (\(ServiceWorkerState _ _ x) -> x)
  subscribe eOpts = ServiceWorkerT $ lift $ tellEvent (Just <$> eOpts)
  unsubscribe e = ServiceWorkerT $ lift $ tellEvent (Nothing <$ e)
  showNotification eNotif = do
    dReg <- getSWRegistration
    ServiceWorkerT $
      performEvent_ $ showNotificationImpl <$> current dReg <@> eNotif
  onNotification = onNotificationImpl

onNotificationImpl :: (Reflex t, TriggerEvent t m, MonadJSM m, FromJSON n) => m (Event t n)
onNotificationImpl = do
  (eMsg, onMsg) <- newTriggerEvent
  liftJSM $ do
    serviceWorker <- jsg ("navigator" :: Text) ^. js ("serviceWorker" :: Text)
    callback <- toJSVal $ fun $ \_ _ [message] -> do
      msgData <- fromJSVal =<< message ^. js ("data" :: Text)
      liftIO $ onMsg (fromMaybe Null msgData)
    msgText <- toJSVal ("message" :: Text)
    void $ serviceWorker ^. jsf ("addEventListener" :: Text) [msgText, callback]
  pure $ fmapMaybe (resultToMaybe . fromJSON) eMsg
  where
    resultToMaybe (Success a) = Just a
    resultToMaybe _ = Nothing

showNotificationImpl ::
     MonadJSM m
  => Maybe ServiceWorkerRegistration
  -> (Text, Maybe NotificationOptions)
  -> m ()
showNotificationImpl Nothing _ = pure ()
showNotificationImpl (Just reg) (title, opts) = liftJSM $ catch
  (void $ unServiceWorkerRegistration reg ^. js2 ("showNotification" :: Text) title (toJSON <$> opts))
  (\(_ :: JSException) -> pure ())

vapidKeyToArray :: MonadJSM m => Text -> m JSVal
vapidKeyToArray x = liftJSM $ do
  let x' = T.replace "-" "+" (T.replace "_" "/" x) <> T.replicate ((4 - T.length x `mod` 4) `mod` 4) "="
  raw <- valToText =<< jsg ("window" :: Text) ^. js1 ("atob" :: Text) x'
  out <- new (jsg ("Uint8Array" :: Text)) [T.length raw]
  itraverse_ (\ix pos -> (out <## ix) pos) (ord <$> T.unpack raw)
  pure out
