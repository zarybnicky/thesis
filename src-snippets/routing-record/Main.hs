{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Warp as JS (run)
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.App
import Servant.API
import Servant.API.Generic

main :: IO ()
main = run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = serve (genericApi $ Proxy @Routes) (toServant widgets) errorPage

errorPage :: MonadWidget t m => Err -> m ()
errorPage = text . T.pack . show

widgets :: (EventWriter t Loc m, MonadWidget t m) => Routes (AsApp t m)
widgets = Routes
  { showUserRoute = \(i :: Int) -> text (T.pack $ show i)
  , homeRoute = do
      appLink showUserRoute =<< (5 <$) <$> button "Go to user"
      e <- ("<>>" <$) <$> button "Go to admin"
      appLink (adminRoute .> listUsersRoute) e
      pure ()
  , adminRoute = toServant adminWidgets
  }

adminWidgets :: (EventWriter t Loc m, MonadWidget t m) => AdminRoutes (AsApp t m)
adminWidgets = AdminRoutes
  { listUsersRoute = \slug -> do
      text slug
      appLink homeRoute =<< button "Home"
  }

data Routes route = Routes
  { showUserRoute :: route :- "user" :> Capture "id" Int :> App
  , homeRoute :: route :- App
  , adminRoute :: route :- "admin" :> ToServantApi AdminRoutes
  } deriving (Generic)

data AdminRoutes route = AdminRoutes
  { listUsersRoute :: route :- Capture "slug" Text :> App
  } deriving (Generic)
