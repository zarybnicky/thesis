{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tapaw.RealWorld.Client.Types
  ( AppState(..)
  , AppStateM
  , Route(..)
  , decodeRoute
  , encodeRoute
  , getApi
  ) where

import Control.Monad.Reader (MonadReader, asks)
import Data.List (uncons)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.These (These)
import Data.Time (UTCTime)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Client, link)
import Servant.Reflex (BaseUrl, Client, client)
import Tapaw.RealWorld.API (ConduitAPI)
import Tapaw.RealWorld.Types (User)

data AppState t = AppState
  { stateNow :: Dynamic t UTCTime
  , stateBaseUrl :: Dynamic t BaseUrl
  , stateUser :: Dynamic t (Maybe User)
  , stateRoute :: Demux t Route
  }

type AppStateM t m
   = ( MonadReader (AppState t) m
     , EventWriter t (These Route (Maybe User)) m
     , MonadWidget t m
     )

getApi :: forall t m a. AppStateM t m => m (Client t m ConduitAPI a)
getApi = client (Proxy @ConduitAPI) (Proxy @m) (Proxy @a) <$> asks stateBaseUrl


data Route
  = RouteHome
  | RouteLogin
  | RouteRegister
  | RouteSettings
  | RouteEditor (Maybe Text)
  | RouteArticle Text
  | RouteProfile Text
  | RouteProfileFavorites Text
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Route where
  (<>) = const id

decodeRoute :: Text -> Route
decodeRoute x = case uncons (drop 1 $ T.splitOn "/" x) of
  Just ("login", _) -> RouteLogin
  Just ("register", _) -> RouteRegister
  Just ("settings", _) -> RouteSettings
  Just ("editor", slug) -> RouteEditor $ maybe Nothing (\_ -> Just $ T.intercalate "" slug) (uncons slug)
  Just ("article", slug) -> RouteArticle (T.intercalate "" slug)
  Just ("profile", rest) -> case uncons rest of
    Nothing -> RouteHome
    Just ("favorites", slug) -> RouteProfileFavorites (T.intercalate "" slug)
    Just (slug, _) -> RouteProfile slug
  Nothing -> RouteHome
  _ -> RouteHome

encodeRoute :: Route -> Text
encodeRoute r = case r of
  RouteHome -> "/"
  RouteLogin -> "/login"
  RouteRegister -> "/register"
  RouteSettings -> "/settings"
  RouteEditor mx -> "/editor" <> maybe "" ("/" <>) mx
  RouteArticle x -> "/article/" <> x
  RouteProfile x -> "/profile/" <> x
  RouteProfileFavorites x -> "/profile/favorites/" <> x
