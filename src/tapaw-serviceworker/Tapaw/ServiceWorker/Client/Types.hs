{-# LANGUAGE DeriveGeneric #-}

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
  ( FromJSON(..)
  , Options(..)
  , ToJSON(..)
  , Value
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSVal)


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
  deriving (Generic)

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
  , applicationServerKey :: Text
  } deriving (Generic)

instance FromJSON PushSubscriptionOptions where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PushSubscriptionOptions where
  toJSON = genericToJSON defaultOptions

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
  } deriving (Generic)

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
  } deriving (Generic)

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
  deriving (Generic)

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
  } deriving (Generic)

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
