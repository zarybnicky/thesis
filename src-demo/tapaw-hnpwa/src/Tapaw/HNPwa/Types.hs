{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapaw.HNPwa.Types
  ( UserId(..)
  , ItemId(..)
  , FilterType(..)
  , ItemType(..)
  , Item(..)
  , User(..)
  , AppRequest(..)
  , AppState(..)
  , Route(..)
  , filterTypeToUrl
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
import Tapaw.HNPwa.Utils
import Text.Read (readMaybe)


newtype UserId = UserId
  { unUserId :: Text
  } deriving (Eq, Ord, Show, FromJSON)


newtype ItemId = ItemId
  { unItemId :: Int
  } deriving (Eq, Ord, Show, FromJSON)


data FilterType
  = FilterBest
  | FilterNew
  | FilterShow
  | FilterAsk
  | FilterJobs
  deriving (Eq, Ord, Show)

filterTypeToUrl :: FilterType -> Text
filterTypeToUrl x = case x of
  FilterBest -> "best"
  FilterNew -> "new"
  FilterShow -> "show"
  FilterAsk -> "ask"
  FilterJobs -> "jobs"


data ItemType
  = ItemStory
  | ItemComment
  | ItemJob
  | ItemPoll
  | ItemPollOpt
  deriving (Eq, Ord, Show)

instance FromJSON ItemType where
  parseJSON (A.String "story") = pure ItemStory
  parseJSON (A.String "comment") = pure ItemComment
  parseJSON (A.String "job") = pure ItemJob
  parseJSON (A.String "poll") = pure ItemPoll
  parseJSON (A.String "pollopt") = pure ItemPollOpt
  parseJSON _ = mempty


data Item = Item
  { itemId :: ItemId
  , itemType :: ItemType
  , itemBy :: UserId
  , itemTime :: UTCTime
  , itemText :: Text
  , itemParent :: Maybe ItemId
  , itemPoll :: Maybe ItemId
  , itemKids :: [ItemId]
  , itemUrl :: Maybe Text
  , itemScore :: Int
  , itemTitle :: Text
  , itemParts :: [ItemId]
  , itemDescendants :: Int
  } deriving (Eq, Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> Item
    <$> o .: "id"
    <*> o .: "type"
    <*> (fromMaybe (UserId "deleted") <$> o .:? "by")
    <*> (posixSecondsToUTCTime <$> o .: "time")
    <*> (fromMaybe "" <$> o .:? "text")
    <*> o .:? "parent"
    <*> o .:? "poll"
    <*> (fromMaybe [] <$> o .:? "kids")
    <*> o .:? "url"
    <*> (fromMaybe 0 <$> o .:? "score")
    <*> (fromMaybe "" <$> o .:? "title")
    <*> (fromMaybe [] <$> o .:? "parts")
    <*> (fromMaybe 0 <$> o .:? "descendants")


data User = User
  { userId :: UserId
  , userCreated :: UTCTime
  , userAbout :: Maybe Text
  , userKarma :: Int
  } deriving Show

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> (posixSecondsToUTCTime <$> o .: "created")
    <*> o .:? "about"
    <*> o .: "karma"


data AppRequest
  = ReqItem ItemId
  | ReqItemList FilterType
  | ReqUser UserId
  deriving (Eq, Ord, Show)


data AppState t = AppState
  { now :: Dynamic t UTCTime
  , itemMap :: Dynamic t (Map ItemId Item)
  , userMap :: Dynamic t (Map UserId User)
  , itemLists :: Dynamic t (Map FilterType [ItemId])
  , pendingReqs :: Dynamic t (Set AppRequest)
  }


data Route
  = RouteItemList FilterType Int
  | RouteItem ItemId
  | RouteUser UserId
  deriving (Show, Generic)

instance Semigroup Route where
  (<>) = const id

decodeRoute :: Text -> Route
decodeRoute x = case uncons (drop 1 $ T.splitOn "/" x) of
  Just ("best", rest) -> RouteItemList FilterBest (parsePageNum rest)
  Just ("new", rest) -> RouteItemList FilterNew (parsePageNum rest)
  Just ("show", rest) -> RouteItemList FilterShow (parsePageNum rest)
  Just ("ask", rest) -> RouteItemList FilterAsk (parsePageNum rest)
  Just ("jobs", rest) -> RouteItemList FilterJobs (parsePageNum rest)
  Just ("item", rest) -> RouteItem (ItemId $ parsePageNum rest)
  Just ("user", rest) -> RouteUser (UserId . T.intercalate "" $ take 1 rest)
  _ -> RouteItemList FilterBest 1
  where
    parsePageNum :: [Text] -> Int
    parsePageNum = max 1 . fromMaybe 1 . readMaybe . T.unpack . T.intercalate "" . take 1

encodeRoute :: Route -> Text
encodeRoute (RouteItemList f p) = "/" <> filterTypeToUrl f <> "/" <> tshow p
encodeRoute (RouteItem u) = "/item/" <> tshow (unItemId u)
encodeRoute (RouteUser u) = "/user/" <> unUserId u
