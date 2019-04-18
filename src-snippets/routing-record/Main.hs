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

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Language.Javascript.JSaddle.Warp as JS (run)
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.App
import Servant.API
import Servant.API.Generic

import Servant.App.AsGenerator

main :: IO ()
main = run 3000 $ mainWidget $ do
  let gens = runGen widgets $ do
        gen showUserRoute 2
        gen homeRoute
        gen (adminRoute .> editUserRoute) "text"

  _ <- runEventWriterT $ for gens $ \(loc, g) -> do
    text $ T.pack (show loc) <> ":"
    el "br" blank
    g
    el "hr" blank
  pure ()

app :: MonadWidget t m => m ()
app = serve (genericApi $ Proxy @Routes) (toServant widgets) errorPage

errorPage :: MonadWidget t m => Err -> m ()
errorPage = text . T.pack . show

widgets :: (EventWriter t Loc m, MonadWidget t m) => Routes (AsApp m)
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

adminWidgets :: (EventWriter t Loc m, MonadWidget t m) => AdminRoutes (AsApp m)
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
