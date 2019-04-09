{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapaw.RealWorld.Client.Types
  ( Route(..)
  , decodeRoute
  , encodeRoute
  ) where

import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

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
