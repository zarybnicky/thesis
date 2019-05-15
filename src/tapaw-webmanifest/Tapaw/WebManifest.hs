{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapaw.WebManifest
  ( WebManifest(..)
  , AppURI(..)
  , TextDirection(..)
  , ImageResource(..)
  , DisplayMode(..)
  , OrientationLock(..)
  , ServiceWorkerRegistrationObject(..)
  , WorkerType(..)
  , ServiceWorkerUpdateViaCache(..)
  , ExternalApplicationResource(..)
  , Fingerprint(..)
  , emptyManifest
  ) where

import Data.Aeson as A
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified  Data.Text as T
import GHC.Generics (Generic)
import Network.URI (URI, parseRelativeReference)

data WebManifest = WebManifest
  { dir :: Maybe TextDirection
  , lang :: Maybe Text
  , name :: Maybe Text
  , short_name :: Maybe Text
  , description :: Maybe Text
  , icons :: [ImageResource]
  , screenshots :: [ImageResource]
  , categories :: [Text]
  , oarc_rating_id :: Maybe Text
  , start_url :: Maybe AppURI
  , display :: Maybe DisplayMode
  , orientation :: Maybe OrientationLock
  , theme_color :: Maybe AppURI
  , background_color :: Maybe Text
  , scope :: Maybe AppURI
  , serviceworker :: Maybe ServiceWorkerRegistrationObject
  , related_applications :: [ExternalApplicationResource]
  , prefer_related_applications :: Maybe Bool
  } deriving (Generic, FromJSON, ToJSON)

emptyManifest :: WebManifest
emptyManifest = WebManifest
  { dir = Nothing
  , lang = Nothing
  , name = Nothing
  , short_name = Nothing
  , description = Nothing
  , icons = []
  , screenshots = []
  , categories = []
  , oarc_rating_id = Nothing
  , start_url = Nothing
  , display = Nothing
  , orientation = Nothing
  , theme_color = Nothing
  , background_color = Nothing
  , scope = Nothing
  , serviceworker = Nothing
  , related_applications = []
  , prefer_related_applications = Nothing
  }

newtype AppURI = AppURI
  { unAppURL :: URI
  } deriving (Generic)

instance FromJSON AppURI where
  parseJSON (A.String x) = maybe mempty pure $ AppURI <$> parseRelativeReference (T.unpack x)
  parseJSON _ = mempty

instance ToJSON AppURI where
  toJSON = String . T.pack . show . unAppURL


data TextDirection
  = Ltr
  | Rtl
  | Auto
  deriving (Generic)

instance FromJSON TextDirection where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON TextDirection where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}


data ImageResource = ImageResource
  { imageResourceSrc :: Text
  , imageResourceSizes :: [(Int, Int)]
  , imageResourceType :: Maybe Text
  , imageResourcePurpose :: Maybe Text
  , imageResourcePlatform :: Maybe Text
  } deriving Generic

instance FromJSON ImageResource where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "imageResource" x}

instance ToJSON ImageResource where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> map toLower $ fromMaybe x $ stripPrefix "imageResource" x}


data DisplayMode
  = Fullscreen
  | Standalone
  | MinimalUi
  | Browser
  deriving Generic

instance FromJSON DisplayMode where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_'}

instance ToJSON DisplayMode where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = camelTo2 '_'}


data OrientationLock
  = Any
  | Natural
  | Landscape
  | Portrait
  | PortraitPrimary
  | PortraitSecondary
  | LandscapePrimary
  | LandscapeSecondary
  deriving Generic

instance FromJSON OrientationLock where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '-'}

instance ToJSON OrientationLock where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = camelTo2 '-'}


data ServiceWorkerRegistrationObject = ServiceWorkerRegistrationObject
  { serviceWorkerRegistrationObjectSrc :: AppURI
  , serviceWorkerRegistrationObjectScope :: Maybe AppURI
  , serviceWorkerRegistrationObjectType :: Maybe WorkerType
  , serviceWorkerRegistrationObjectUpdateViaCache :: Maybe ServiceWorkerUpdateViaCache
  } deriving Generic

instance FromJSON ServiceWorkerRegistrationObject where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "serviceWorkerRegistrationObject" x}

instance ToJSON ServiceWorkerRegistrationObject where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "serviceWorkerRegistrationObject" x}


data WorkerType
  = Classic
  | Module
  deriving Generic

instance FromJSON WorkerType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON WorkerType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}


data ServiceWorkerUpdateViaCache
  = Imports
  | All
  | None
  deriving Generic

instance FromJSON ServiceWorkerUpdateViaCache where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = map toLower}

instance ToJSON ServiceWorkerUpdateViaCache where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}


data ExternalApplicationResource = ExternalApplicationResource
  { externalApplicationResourcePlatform :: Text
  , externalApplicationResourceUrl :: Maybe AppURI
  , externalApplicationResourceId :: Maybe Text
  , externalApplicationResourceMinVersion :: Maybe Text
  , externalApplicationResourceFingerprints :: [Fingerprint]
  } deriving Generic

instance FromJSON ExternalApplicationResource where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "externalApplicationResource" x}

instance ToJSON ExternalApplicationResource where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "externalApplicationResource" x}


data Fingerprint = Fingerprint
  { fingerprintType :: Maybe Text
  , fingerprintValue :: Maybe Text
  } deriving Generic

instance FromJSON Fingerprint where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "fingerprint" x}

instance ToJSON Fingerprint where
  toJSON =
    genericToJSON
      defaultOptions
        {fieldLabelModifier = \x -> camelTo2 '_' . fromMaybe x $ stripPrefix "fingerprint" x}
