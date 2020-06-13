{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.API
import Servant.API.Generic
import Tapaw.Servant
import Tapaw.Servant.AsGenerator
import URI.ByteString

main :: IO ()
main = runGen "out" (runRoutedTFrozen (baseUri, Loc [] [])) appHead widgets $ do
  gen showUserRoute 2
  gen homeRoute
  gen (adminRoute .> editUserRoute) "text"
  where
    baseUri :: URIRef Absolute
    baseUri = fromRight undefined $ parseURI laxURIParserOptions "http://www.example.org"

appHead :: DomBuilder t m => m ()
appHead = el "title" $ text "Static app"

app :: MonadWidget t m => m ()
app = do
  i <- fromRight undefined <$> getInitialRouteHistory
  runRoutedTHistory i $ do
    _ <- runRouterM widgets errorPage
    pure ()

errorPage :: DomBuilder t m => Err -> m ()
errorPage = text . T.pack . show

widgets :: (DomBuilder t m, PostBuild t m, MonadRouted Routes t m) => Routes (AsApp (m ()))
widgets = Routes
  { showUserRoute = \i ->
      text (T.pack $ show i)
  , homeRoute = do
      appLink showUserRoute 5 (text "Go to user")
      appLink (adminRoute .> editUserRoute) "..." (text "Go to admin")
      pure ()
  , adminRoute = toServant adminWidgets
  }

adminWidgets :: (DomBuilder t m, PostBuild t m, MonadRouted Routes t m) => AdminRoutes (AsApp (m ()))
adminWidgets = AdminRoutes
  { listUsersRoute = do
      text "users"
      appLink homeRoute () (text "Home")
  , editUserRoute = \slug -> do
      text slug
      appLink homeRoute () (text "Home")
  }

data Routes route = Routes
  { showUserRoute :: route :- "user" :> Capture "id" Int :> App
  , homeRoute :: route :- App
  , adminRoute :: route :- ToServantApi AdminRoutes
  } deriving (Generic)

data AdminRoutes route = AdminRoutes
  { listUsersRoute :: route :- "admin" :> App
  , editUserRoute :: route :- "admin" :> Capture "slug" Text :> App
  } deriving (Generic)
