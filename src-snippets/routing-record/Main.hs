{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Language.Javascript.JSaddle.Warp as JS (run)
import GHC.Generics (Generic)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.App
import Servant.API

main :: IO ()
main = run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = serve (Proxy @(ToServantApi Routes)) (toServant widgets) (text . T.pack . show)

widgets :: forall t m. MonadWidget t m => Routes (AsApp t (EventWriterT t Loc m))
widgets = Routes
  { showUserRoute = \(i :: Int) -> text (T.pack $ show i)
  , homeRoute = do
      e <- button "Go to user"
      appLink showUserRoute (5 <$ e)
  }

data Routes route = Routes
  { showUserRoute :: route :- "user" :> Capture "id" Int :> App
  , homeRoute :: route :- App
  } deriving (Generic)
