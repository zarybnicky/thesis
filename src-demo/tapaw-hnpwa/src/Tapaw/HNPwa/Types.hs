{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tapaw.HNPwa.Types
  ( UserId(..)
  , ItemId(..)
  , FilterType(..)
  , ItemType(..)
  , Item(..)
  , User(..)
  , AppRequest(..)
  , AppState(..)
  , AppRoute(..)
  , Route(..)
  ) where

import Data.Aeson as A
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Reflex.Dom.Core
import Servant.API
import Servant.API.Generic
import Tapaw.Servant


newtype UserId = UserId
  { unUserId :: Text
  } deriving (Eq, Ord, Show, FromJSON, FromHttpApiData, ToHttpApiData)


newtype ItemId = ItemId
  { unItemId :: Int
  } deriving (Eq, Ord, Show, FromJSON, FromHttpApiData, ToHttpApiData)


data FilterType
  = FilterBest
  | FilterNew
  | FilterShow
  | FilterAsk
  | FilterJobs
  deriving (Eq, Ord, Show)

instance FromHttpApiData FilterType where
  parseUrlPiece "best" = Right FilterBest
  parseUrlPiece "new" = Right FilterNew
  parseUrlPiece "show" = Right FilterShow
  parseUrlPiece "ask" = Right FilterAsk
  parseUrlPiece "jobs" = Right FilterJobs
  parseUrlPiece x = Left x

instance ToHttpApiData FilterType where
  toUrlPiece FilterBest = "best"
  toUrlPiece FilterNew = "new"
  toUrlPiece FilterShow = "show"
  toUrlPiece FilterAsk = "ask"
  toUrlPiece FilterJobs = "jobs"


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

data Route
  = RouteItemList FilterType Int
  | RouteItem ItemId
  | RouteUser UserId
  deriving (Show, Generic)

data AppState t = AppState
  { now :: Dynamic t UTCTime
  , itemMap :: Dynamic t (Map ItemId Item)
  , userMap :: Dynamic t (Map UserId User)
  , itemLists :: Dynamic t (Map FilterType [ItemId])
  , pendingReqs :: Dynamic t (Set AppRequest)
  }

data AppRoute r = AppRoute
  { rList :: r :- Capture "type" FilterType :> Capture "page" Int :> App
  , rItem :: r :- "item" :> Capture "item" ItemId :> App
  , rUser :: r :- "user" :> Capture "user" UserId :> App
  } deriving (Generic)
