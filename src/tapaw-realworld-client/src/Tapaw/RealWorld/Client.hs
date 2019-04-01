{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapaw.RealWorld.Client
  ( frontend
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (UTCTime(..), getCurrentTime)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import Tapaw.RealWorld.Client.Navigation (makeHistoryRouter, appLink, appLinkDyn)
import Tapaw.RealWorld.Client.Types
import Tapaw.RealWorld.Client.Utils ((<!>), (=?), (=!), tshow)


data AppState t = AppState
  { now :: Dynamic t UTCTime
  }

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" (text "RealWorld")
  elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width,initial-scale=1") blank
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/index.css") blank

frontend :: JSM ()
frontend = mainWidgetWithHead headWidget $ do
  t0 <- liftIO getCurrentTime
  dNow <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0

  rec
    let appState = AppState dNow

    dRoute <- makeHistoryRouter (RouteUser "x") eSetRoute
    ((), eSetRoute) <- flip runReaderT appState . runEventWriterT $
      topLevel dRoute . void . dyn . ffor dRoute $ \case
        RouteUser uid -> userView uid
  blank

topLevel ::
     (EventWriter t Route m, Reflex t, DomBuilder t m, PostBuild t m)
  => Dynamic t Route
  -> m ()
  -> m ()
topLevel dRoute contents =
  elAttr "div" ("id" =: "app") $ do
    elClass "header" "header" $
      elClass "nav" "inner" $ do
        let sel = demux (routeToFilter <$> dRoute)
        appLink (RouteUser "x") (pure mempty) (pure True)
          (elAttr "img" ("class" =: "logo" <> "alt" =: "Logo" <> "src" =: "/favicon.ico") blank)
        appLink (RouteUser "y") (demuxActive sel "y") (pure True) (text "Top")
        elAttr "a" ("class" =: "github" <> "href" =: "https://github.com/zarybnicky/thesis" <>
                    "target" =: "_blank" <> "rel" =: "noopener") (text "Built with Reflex")
    divClass "view" contents
  where
    demuxActive sel f = bool mempty ("class" =: "router-link-active") <$> demuxed sel (Just f)
    routeToFilter (RouteUser x) = Just x

userView :: (MonadReader (AppState t) m, MonadWidget t m) => Text -> m ()
userView uid = do
  let dUserMap = constDyn M.empty
  void . dyn . ffor (M.lookup uid <$> dUserMap) $ \case
    Nothing -> el "h1" (text "User not found.")
    Just (_ :: Text) -> do
      el "h1" (text $ "User : " <> uid)
      elClass "p" "links" $ do
        elAttr "a" ("href" =: ("https://news.ycombinator.com/submitted?id=" <> uid)) (text "submissions")
        text " | "
        elAttr "a" ("href" =: ("https://news.ycombinator.com/threads?id=" <> uid)) (text "comments")

spinner :: MonadWidget t m => Dynamic t Bool -> m ()
spinner dShow =
  elDynAttrNS svgNS "svg" ("class" =: "spinner" <!> baseAttrs <!> "style" =! (bool "display:none" "" <$> dShow)) $
    elDynAttrNS svgNS "circle" (pure circleAttrs) blank
  where
    baseAttrs, circleAttrs :: Map Text Text
    baseAttrs = "width" =: "44px" <> "height" =: "44px" <> "viewBox" =: "0 0 44 44"
    circleAttrs = "class" =: "path" <> "fill" =: "none" <> "cx" =: "22" <>
                   "cy" =: "22" <> "r" =: "20" <> "stroke-width" =: "4" <>
                   "stroke-linecap" =: "round"

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/"
