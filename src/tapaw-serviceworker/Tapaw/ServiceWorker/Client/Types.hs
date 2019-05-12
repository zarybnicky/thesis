{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapaw.ServiceWorker.Client.Types
  ( ServiceWorkerRegistration(..)
  , PushSubscription(..)
  , PermissionState(..)
  , PushSubscriptionOptions(..)
  , NotificationOptions(..)
  , NotificationAction(..)
  , TextDirection(..)
  , ServiceWorkerOptions(..)
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle


newtype ServiceWorkerRegistration = ServiceWorkerRegistration
  { unServiceWorkerRegistration :: JSVal
  }

newtype PushSubscription = PushSubscription
  { unPushSubscription :: JSVal
  }

data PermissionState
  = PermissionGranted
  | PermissionDenied
  | PermissionPrompt
  deriving (Show, Generic)

instance FromJSON PermissionState where
  parseJSON =
    genericParseJSON
      defaultOptions
        {constructorTagModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "Permission" x}

instance ToJSON PermissionState where
  toJSON =
    genericToJSON
      defaultOptions
        {constructorTagModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "Permission" x}


data PushSubscriptionOptions = PushSubscriptionOptions
  { userVisibleOnly :: Bool
  , applicationServerKey :: Maybe JSVal
  } deriving (Generic)

instance ToJSVal PushSubscriptionOptions where
  toJSVal v = do
    o <- create
    setProp "userVisibleOnly" (toJSBool $ userVisibleOnly v) o
    case applicationServerKey v of
      Nothing -> pure ()
      Just x -> setProp "applicationServerKey" x o
    toJSVal o

instance Semigroup PushSubscriptionOptions where
  a <> _ = a


data NotificationOptions = NotificationOptions
  { notificationActions :: Maybe [NotificationAction]
  , notificationBadge :: Maybe Text
  , notificationBody :: Maybe Text
  , notificationData :: Maybe Value
  , notificationDir :: Maybe TextDirection
  , notificationIcon :: Maybe Text
  , notificationImage :: Maybe Text
  , notificationLang :: Maybe Text
  , notificationRenotify :: Maybe Bool
  , notificationRequireInteraction :: Maybe Bool
  , notificationSilent :: Maybe Bool
  , notificationTag :: Maybe Text
  , notificationTimestamp :: Maybe Int
  , notificationVibrate :: Maybe [Int]
  } deriving (Show, Generic)

instance FromJSON NotificationOptions where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "notification" x}

instance ToJSON NotificationOptions where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "notification" x}


data NotificationAction = NotificationAction
  { notificationActionAction :: Maybe Text
  , notificationActionTitle :: Maybe Text
  , notificationActionIcon :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON NotificationAction where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "notificationAction" x}

instance ToJSON NotificationAction where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "notificationAction" x}


data TextDirection
  = TextDirectionAuto
  | TextDirectionLtr
  | TextDirectionRtl
  deriving (Show, Generic)

instance FromJSON TextDirection where
  parseJSON =
    genericParseJSON
      defaultOptions
        {constructorTagModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "TextDirection" x}

instance ToJSON TextDirection where
  toJSON =
    genericToJSON
      defaultOptions
        {constructorTagModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "TextDirection" x}


newtype ServiceWorkerOptions = ServiceWorkerOptions
  { serviceWorkerScope :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ServiceWorkerOptions where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "serviceWorker" x}

instance ToJSON ServiceWorkerOptions where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "serviceWorker" x}
