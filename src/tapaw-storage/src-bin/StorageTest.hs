{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Language.Javascript.JSaddle.Warp (run)
import GHC.Generics (Generic)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Window (getLocalStorage)
import Reflex.Dom.Core
import Tapaw.Storage (StoreKey, getKVAll, putKV, runKVStoreTStorage)

main :: IO ()
main = run 3000 $ mainWidget $ do
  storage <- getLocalStorage =<< currentWindowUnchecked
  runKVStoreTStorage @User storage "users" $ do
    eIns <- button "Insert"
    eDel <- button "Delete"
    putKV ((UserId 5, Just User) <$ eIns)
    putKV ((UserId 5, Nothing) <$ eDel)
    display =<< getKVAll @User

data User = User
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype UserId = UserId
  { unUserId :: Int
  } deriving newtype (Eq, Ord, Show, FromJSONKey, ToJSONKey)

type instance StoreKey User = UserId
