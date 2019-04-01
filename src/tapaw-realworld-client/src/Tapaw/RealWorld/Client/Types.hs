{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapaw.RealWorld.Client.Types
  ( Route(..)
  , decodeRoute
  , encodeRoute
  ) where

import Data.Aeson as A
import Data.List (uncons)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Reflex.Dom.Core
import Text.Read (readMaybe)

data Route
  = RouteUser Text
  deriving (Show, Generic)

instance Semigroup Route where
  (<>) = const id

decodeRoute :: Text -> Route
decodeRoute x = case uncons (drop 1 $ T.splitOn "/" x) of
  Just ("user", rest) -> RouteUser (T.intercalate "" $ take 1 rest)
  _ -> RouteUser ""

encodeRoute :: Route -> Text
encodeRoute (RouteUser u) = "/user/" <> u
