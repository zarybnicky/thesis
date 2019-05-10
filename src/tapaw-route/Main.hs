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

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Warp as JS (run)
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.API
import Servant.API.Generic
import Tapaw.Servant

main :: IO ()
main = runGen "out" runEventWriterT appHead widgets $ do
  gen showUserRoute 2
  gen homeRoute
  gen (adminRoute .> editUserRoute) "text"

appHead :: DomBuilder t m => m ()
appHead = el "title" $ text "Static app"

app :: MonadWidget t m => m ()
app = serve (genericApi $ Proxy @Routes) (toServant widgets) errorPage

errorPage :: DomBuilder t m => Err -> m ()
errorPage = text . T.pack . show

widgets :: (EventWriter t Loc m, DomBuilder t m) => Routes (AsApp m)
widgets = Routes
  { showUserRoute = \i ->
      text (T.pack $ show i)
  , homeRoute = do
      e' <- button "Go to user"
      appLink showUserRoute (5 <$ e')
      e <- ("..." <$) <$> button "Go to admin"
      appLink (adminRoute .> editUserRoute) e
      pure ()
  , adminRoute = toServant adminWidgets
  }

adminWidgets :: (EventWriter t Loc m, DomBuilder t m) => AdminRoutes (AsApp m)
adminWidgets = AdminRoutes
  { listUsersRoute = do
      text "users"
      appLink homeRoute =<< button "Home"
  , editUserRoute = \slug -> do
      text slug
      appLink homeRoute =<< button "Home"
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
