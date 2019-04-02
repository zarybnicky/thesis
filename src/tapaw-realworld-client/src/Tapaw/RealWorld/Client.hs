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
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
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
  elAttr "meta" ("charset" =: "utf-8") blank
  el "title" (text "Conduit")
  elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width,initial-scale=1") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic") blank
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
        RouteUser _ -> blank
  blank

topLevel ::
     (EventWriter t Route m, MonadWidget t m) => Dynamic t Route -> m () -> m ()
topLevel dRoute contents =
  elAttr "div" ("id" =: "app") $ do
    headerView dRoute
    contents
    footerView

headerView :: (EventWriter t Route m, MonadWidget t m) => Dynamic t Route -> m ()
headerView dRoute =
  elClass "nav" "navbar navbar-light" $ divClass "container" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "/index.html") (text "conduit")
    elClass "ul" "nav navbar-nav pull-xs-right" $ do
      let sel = demux dRoute
      -- TODO: Add "active" class when you're on that page
      elClass "li" "nav-item" $
        appLink (RouteUser "x") (demuxActive sel (RouteUser "x")) (pure True) (text "Home")
      elClass "li" "nav-item" $
        appLink (RouteUser "y") (demuxActive sel (RouteUser "y")) (pure True) $ do
          elClass "i" "ion-compose" blank
          nbsp
          text "New Post"
      elClass "li" "nav-item" $
        appLink (RouteUser "z") (demuxActive sel (RouteUser "z")) (pure True) $ do
          elClass "i" "ion-gear-a" blank
          nbsp
          text "Settings"
      elClass "li" "nav-item" $
        appLink (RouteUser "z") (demuxActive sel (RouteUser "z")) (pure True) $
          text "Sign up"
  where
    demuxActive sel f = ("class" =:) . bool "nav-link" "nav-link active" <$> demuxed sel f

footerView :: MonadWidget t m => m ()
footerView =
  el "footer" $ divClass "container" $ do
    elAttr "a" ("class" =: "logo-font" <> "href" =: "/") (text "conduit")
    elClass "span" "attribution" $ do
      text "An interactive learning project from "
      elAttr "a" ("href" =: "https://thinkster.io") (text "Thinkster")
      text ". Code & design licensed under MIT."

homePage :: MonadWidget t m => m ()
homePage = divClass "home-page" $ do
  divClass "banner" $ divClass "container" $ do
    elClass "h1" "logo-font" (text "conduit")
    el "p" (text "A place to share your knowledge.")
  divClass "container page" $ divClass "row" $ do
    divClass "col-md-9" $ do
      divClass "feed-toggle" $ elClass "ul" "nav nav-pills outline-active" $ do
        elClass "li" "nav-item" $
          elAttr "a" ("class" =: "nav-link disabled" <> "href" =: "") (text "Your Feed")
        elClass "li" "nav-item" $
          elAttr "a" ("class" =: "nav-link active" <> "href" =: "") (text "Global Feed")

      divClass "article-preview" $ do
        divClass "article-meta" $ do
          elAttr "a" ("href" =: "profile.html") $
            elAttr "img" ("src"  =: "http://i.imgur.com/Qr71crq.jpg") blank
          divClass "info" $ do
            elAttr "a" ("class" =: "author" <> "href" =: "") (text "Eric Simons")
            elClass "span" "date" (text "January 20th")
          elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
            elClass "i" "ion-heart" blank
            text "29"
        elAttr "a" ("class" =: "preview-link" <> "href" =: "") $ do
          el "h1" (text "How to build webapps that scale")
          el "p" (text "This is the description for the post.")
          el "span" (text "Read more...")

      divClass "article-preview" $ do
        divClass "article-meta" $ do
          elAttr "a" ("href" =: "/profile.html") $
            elAttr "img" ("src" =: "http://i.imgur.com/N4VcUeJ.jpg") blank
          divClass "info" $ do
            elAttr "a" ("href" =: "" <> "class" =: "author") (text "Albert Pai")
            elClass "span" "date" (text "January 20th")
          elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
            elClass "i" "ion-heart" blank
            text "32"
        elAttr "a" ("href" =: "" <> "class" =: "preview-link") $ do
          el "h1" (text "The song you won't ever stop singing. No matter how hard you try.")
          el "p" (text "This is the description for the post.")
          el "span" (text "Read more...")

    divClass "col-md-3" $ divClass "sidebar" $ do
      el "p" (text "Popular Tags")
      elClass "div" "tag-list" $ do
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "programming")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "emberjs")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "angularjs")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "react")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "mean")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "node")
        elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (text "rails")

loginRegisterPage :: MonadWidget t m => m ()
loginRegisterPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ do
    elClass "h1" "text-xs-center" (text "Sign up")
    elClass "p" "text-xs-center" $ elAttr "a" ("href" =: "") (text "Have an account?")
    elClass "ul" "error-messages" $ el "li" (text "That email is already taken")

    el "form" $ do
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "text" <> "placeholder" =: "Your Name") blank
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "text" <> "placeholder" =: "Email") blank
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "password" <> "placeholder" =: "Password") blank
      elClass "button" "btn btn-lg btn-primary pull-xs-right" (text "Sign up")

profilePage :: MonadWidget t m => m ()
profilePage = divClass "profile-page" $ do
  divClass "user-info" $ divClass "container" $
      divClass "row" $ divClass "col-xs-12 col-md-10 offset-md-1" $ do
    elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg" <> "class" =: "user-img") blank
    el "h4" (text "Eric Simons")
    el "p" (text "Cofounder @GoThinkster, lived in Aol's HQ for a few months, kinda looks like Peeta from the Hunger Games")
    elClass "button" "btn btn-sm btn-outline-secondary action-btn" $ do
      elClass "i" "ion-plus-round" blank
      nbsp
      text "Follow Eric Simons"

  divClass "container" $ divClass "row" $
    divClass "col-xs-12 col-md-10 offset-md-1" $ do
      divClass "articles-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $
            elAttr "a" ("class" =: "nav-link active" <> "href" =: "") (text "My Articles")
          elClass "li" "nav-item" $
            elAttr "a" ("class" =: "nav-link" <> "href" =: "") (text "Favorited Articles")

      divClass "articles-preview" $ do
        divClass "article-meta" $ do
          elAttr "a" ("href" =: "") $ elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg") blank
          divClass "info" $ do
            elAttr "a" ("href" =: "" <> "class" =: "author") (text "Eric Simons")
            elClass "span" "date" (text "January 20th")
          elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
            elClass "i" "ion-heart" blank
            text "29"
        elAttr "a" ("class" =: "preview-link" <> "href" =: "") $ do
          el "h1" (text "How to build webapps that scale")
          el "p" (text "This is the description for the post.")
          el "span" (text "Read more...")

      divClass "article-preview" $ do
        divClass "article-meta" $ do
          elAttr "a" ("href" =: "") $ elAttr "img" ("src" =: "http://i.imgur.com/N4VcUeJ.jpg") blank
          divClass "info" $ do
            elAttr "a" ("href" =: "" <> "class" =: "author") (text "Albert Pai")
            elClass "span" "date" (text "January 20th")
          elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
            elClass "i" "ion-heart" blank
            text "32"
        elAttr "a" ("class" =: "preview-link" <> "href" =: "") $ do
          el "h1" (text "The song you won't ever stop singing. No matter how hard you try.")
          el "p" (text "This is the description for the post.")
          el "span" (text "Read more...")
          elClass "ul" "tag-list" $ do
            elClass "li" "tag-default tag-pill tag-outline" (text "Music")
            elClass "li" "tag-default tag-pill tag-outline" (text "Song")

settingsPage :: MonadWidget t m => m ()
settingsPage =
  divClass "settings-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ do
    elClass "h1" "text-xs-center" (text "Your Settings")
    el "form" $ el "fieldset" $ do
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control" <>
                        "type" =: "text" <> "placeholder" =: "URL of profile picture") blank
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "text" <> "placeholder" =: "Your Name") blank
      elClass "fieldset" "form-group" $
        elAttr "textarea" ("class" =: "form-control form-control-lg" <>
                           "rows" =: "8" <> "placeholder" =: "Short bio about you") blank
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "text" <> "placeholder" =: "Email") blank
      elClass "fieldset" "form-group" $
        elAttr "input" ("class" =: "form-control form-control-lg" <>
                        "type" =: "password" <> "placeholder" =: "Password") blank
      elClass "button" "btn btn-lg btn-primary pull-xs-right" (text "Update Settings")

createEditArticlePage :: MonadWidget t m => m ()
createEditArticlePage =
  divClass "editor-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-10 offset-md-1 col-xs-12" $
      el "form" $ el "fieldset" $ do
    elClass "fieldset" "form-group" $
      elAttr "input" ("type" =: "text" <> "class" =: "form-control form-control-lg" <>
                      "placeholder" =: "Article Title") blank
    elClass "fieldset" "form-group" $
      elAttr "input" ("type" =: "text" <> "class" =: "form-control" <>
                      "placeholder" =: "What's this article about?") blank
    elClass "fieldset" "form-group" $
      elAttr "textarea" ("class" =: "form-control" <> "rows" =: "8" <>
                         "placeholder" =: "Write your article (in markdown)") blank
    elClass "fieldset" "form-group" $ do
      elAttr "input" ("type" =: "text" <> "class" =: "form-control" <>
                      "placeholder" =: "Enter tags") blank
      divClass "tag-list" blank
    elClass "button" "btn btn-lg btn-primary pull-xs-right" (text "Publish Article")

articlePage :: MonadWidget t m => m ()
articlePage = divClass "article-page" $ do
  divClass "banner" $ divClass "container" $ do
    el "h1" (text "How to build webapps that scale")
    divClass "article-meta" $ do
      elAttr "a" ("href" =: "") $
        elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg") blank
      divClass "info" $ do
        elAttr "a" ("href" =: "" <> "class" =: "author") (text "Eric Simons")
        elClass "span" "date" (text "January 20th")
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-plus-round" blank
        nbsp
        text "Follow Eric Simons "
        elClass "span" "counter" (text "(10)")
      nbsp >> nbsp
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-heart" blank
        nbsp
        text "Favorite Post "
        elClass "span" "counter" (text "(29)")

  divClass "container page" $ do
    divClass "row article-content" $ divClass "col-md-12" $ do
      el "p" (text "Web development technologies have evolved at an incredible clip over the past few years.")
      elAttr "h2" ("id" =: "introducing-ionic") (text "Introducing RealWorld.")
      el "p" (text "It's a great solution for learning how other frameworks work.")

    el "hr" blank

    divClass "article-actions" $ divClass "article-meta" $ do
      elAttr "a" ("href" =: "") $
        elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg") blank
      divClass "info" $ do
        elAttr "a" ("href" =: "" <> "class" =: "author") (text "Eric Simons")
        elClass "span" "date" (text "January 20th")
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-plus-round" blank
        nbsp
        text "Follow Eric Simons "
        elClass "span" "counter" (text "(10)")
      nbsp
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-heart" blank
        nbsp
        text "Favorite Post "
        elClass "span" "counter" (text "(29)")

    divClass "row" $ divClass "col-xs-12 col-md-8 offset-md-2" $ do
      elClass "form" "card comment-form" $ do
        divClass "card-block" $
          elAttr "textarea" ("class" =: "form-control" <> "placeholder" =: "Write a comment..." <> "rows" =: "3") blank
        divClass "card-footer" $ do
          elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg" <> "class" =: "comment-author-tag") blank
          elClass "button" "btn btn-sm btn-primary" (text "Post Comment")

      divClass "card" $ do
        divClass "card-block" $
          elClass "p" "card-text" (text "With supporting text below as a natural lead-in to additional content.")
        divClass "card-footer" $ do
          elAttr "a" ("href" =: "" <> "class" =: "comment-author") $
            elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg" <> "class" =: "comment-author-img") blank
          nbsp
          elAttr "a" ("href" =: "" <> "class" =: "comment-author") (text "Jacob Schmidt")
          elClass "span" "date-posted" (text "Dec 29th")

      divClass "card" $ do
        divClass "card-block" $
          elClass "p" "card-text" (text "With supporting text below as a natural lead-in to additional content.")
        divClass "card-footer" $ do
          elAttr "a" ("href" =: "" <> "class" =: "comment-author") $
            elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg" <> "class" =: "comment-author-img") blank
          nbsp
          elAttr "a" ("href" =: "" <> "class" =: "comment-author") (text "Jacob Schmidt")
          elClass "span" "date-posted" (text "Dec 29th")
          elClass "span" "mod-options" $ do
            elClass "i" "ion-edit" blank
            elClass "i" "ion-trash-a" blank

nbsp :: DomBuilder t m => m ()
nbsp = text . T.singleton $ chr 160
