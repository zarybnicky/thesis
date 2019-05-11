{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Tapaw.ServiceWorker.Client
  ( MonadServiceWorker(..)
  , ServiceWorkerT(..)
  , ServiceWorkerState(..)
  , runServiceWorkerT

  , hasServiceWorkerSupport
  , onServiceWorkerMessage
  , registerServiceWorker
  , fetchPushSubscription
  , fetchPushPermissionState
  , showNotificationImpl
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson (Result(Success), Value(Null), fromJSON, toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle hiding (Success)
import Reflex.Dom.Core hiding (Value)
import Tapaw.ServiceWorker.Client.Types

hasServiceWorkerSupport :: MonadJSM m => m Bool
hasServiceWorkerSupport = liftJSM $ do
  a <- ghcjsPure . isTruthy =<< jsg ("navigator" :: Text) ! ("serviceWorker" :: Text)
  b <- ghcjsPure . isTruthy =<< jsg ("window" :: Text) ! ("PushManager" :: Text)
  pure (a && b)

onServiceWorkerMessage :: (TriggerEvent t m, MonadJSM m) => m (Event t Value)
onServiceWorkerMessage = do
  (eMsg, onMsg) <- newTriggerEvent
  liftJSM $ do
    serviceWorker <- jsg ("navigator" :: Text) ^. js ("serviceWorker" :: Text)
    callback <- toJSVal $ fun $ \_ _ [message] -> do
      msgData <- fromJSVal =<< message ^. js ("data" :: Text)
      liftIO $ onMsg (fromMaybe Null msgData)
    msgText <- toJSVal ("message" :: Text)
    void $ serviceWorker ^. jsf ("addEventListener" :: Text) [msgText, callback]
  pure eMsg

registerServiceWorker ::
     (TriggerEvent t m, MonadJSM m)
  => Text
  -> ServiceWorkerOptions
  -> m (Event t ServiceWorkerRegistration)
registerServiceWorker swUrl swOpts = do
  (eReg, onReg) <- newTriggerEvent
  _ <- liftJSM $ do
    serviceWorker <- jsg ("navigator" :: Text) ^. js ("serviceWorker" :: Text)
    callback <- toJSVal $ fun $ \_ _ [swReg] ->
      liftIO $ onReg (ServiceWorkerRegistration swReg)
    res <- serviceWorker ^. jsf ("register" :: Text) [toJSON swUrl, toJSON swOpts]
    res ^. jsf ("then" :: Text) [callback]
  pure eReg

fetchPushSubscription ::
     (MonadJSM m)
  => (Maybe PushSubscription -> IO ())
  -> ServiceWorkerRegistration
  -> m ()
fetchPushSubscription cb reg = liftJSM $ do
  mgr <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text)
  callback <- toJSVal $ fun $ \_ _ [sub] -> do
    sub' <- maybeNullOrUndefined sub
    liftIO $ cb (PushSubscription <$> sub')
  res <- mgr ^. js0 ("getSubscription" :: Text)
  _ <- res ^. jsf ("then" :: Text) [callback]
  pure ()

fetchPushPermissionState ::
     MonadJSM m
  => (PermissionState -> IO ())
  -> ServiceWorkerRegistration
  -> m ()
fetchPushPermissionState cb reg = liftJSM $ do
  mgr <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text)
  callback <- toJSVal $ fun $ \_ _ [perm] -> do
    perm' <- fromJSVal perm
    liftIO $ cb (maybe PermissionDenied (fromSuccess PermissionDenied . fromJSON) perm')
  res <- mgr ^. js0 ("permissionState" :: Text)
  _ <- res ^. jsf ("then" :: Text) [callback]
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

newtype ServiceWorkerT t m a = ServiceWorkerT
  { unServiceWorkerT :: ReaderT (ServiceWorkerState t) (EventWriterT t (Maybe PushSubscriptionOptions) m) a
  } deriving (Functor, Applicative, Monad)

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
  -> ServiceWorkerT t m a
  -> m a
runServiceWorkerT swUrl swOpts f = do
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
      (Nothing, _, _) -> pure ()
      (Just _, Nothing, Nothing) -> pure ()
      (Just reg, Just sub, Nothing) -> liftJSM $ do
        res <- unPushSubscription sub ^. js0 ("unsubscribe" :: Text)
        refreshSubPerm reg onSub onPerm res
      (Just reg, _, Just subOpts) -> liftJSM $ do
        res <- unServiceWorkerRegistration reg ^. js1 ("subscribe" :: Text) (toJSON subOpts)
        refreshSubPerm reg onSub onPerm res
    refreshSubPerm reg onSub onPerm res = do
      subCb <- toJSVal $ fun $ \_ _ [] -> fetchPushSubscription onSub reg
      permCb <- toJSVal $ fun $ \_ _ [] -> fetchPushPermissionState onPerm reg
      _ <- res ^. jsf ("then" :: Text) [subCb]
      _ <- res ^. jsf ("then" :: Text) [permCb]
      pure ()

class MonadServiceWorker t m where
  getSWRegistration :: m (Dynamic t (Maybe ServiceWorkerRegistration))
  getPushSubscription :: m (Dynamic t (Maybe PushSubscription))
  getPushPermissionState :: m (Dynamic t PermissionState)
  subscribe :: Event t PushSubscriptionOptions -> m () -- tellEvent (Maybe PushSubscriptionOptions)
  unsubscribe :: Event t () -> m ()
  showNotification :: Event t (Text, Maybe NotificationOptions) -> m ()

instance (Monad m, Reflex t, PerformEvent t m, MonadJSM (Performable m)) =>
         MonadServiceWorker t (ServiceWorkerT t m) where
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

showNotificationImpl ::
     MonadJSM m
  => Maybe ServiceWorkerRegistration
  -> (Text, Maybe NotificationOptions)
  -> m ()
showNotificationImpl Nothing _ = pure ()
showNotificationImpl (Just reg) (title, opts) = liftJSM $ do
  mgr <- unServiceWorkerRegistration reg ^. js ("pushManager" :: Text)
  _ <- mgr ^. js2 ("showNotification" :: Text) title (toJSON <$> opts)
  pure ()
