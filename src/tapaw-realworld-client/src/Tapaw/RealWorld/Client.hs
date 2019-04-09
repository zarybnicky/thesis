{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tapaw.RealWorld.Client
  ( frontend
  ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Bool (bool)
import Data.Char (chr)
import Data.Generics.Product (field)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), getCurrentTime)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core hiding (Client)
import Servant.Reflex (BaseUrl(..), Client, QParam(..), ReqResult(..), Scheme(..), client, reqSuccess)
import Tapaw.RealWorld.API (ConduitAPI)
import Tapaw.RealWorld.Types
import Tapaw.RealWorld.Client.API
import Tapaw.RealWorld.Client.Navigation (makeHistoryRouter, appLink, appLinkDyn)
import Tapaw.RealWorld.Client.Types
import Tapaw.RealWorld.Client.Utils ((=!), tshow)


data AppState t = AppState
  { stateNow :: Dynamic t UTCTime
  , stateBaseUrl :: Dynamic t BaseUrl
  , stateUser :: Dynamic t (Maybe User)
  , stateRoute :: Demux t Route
  }

type AppStateM t m = (MonadReader (AppState t) m, EventWriter t Route m, MonadWidget t m)

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
frontend = mainWidgetWithHead headWidget app

getApi :: forall t m. AppStateM t m => m (Client t m ConduitAPI ())
getApi = client (Proxy @ConduitAPI) (Proxy @m) (Proxy @()) <$> asks stateBaseUrl

app :: forall t m. MonadWidget t m => m ()
app = do
  t0 <- liftIO getCurrentTime
  dNow <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0
  let url = constDyn (BaseFullUrl Http "localhost" 2990 "/api") :: Dynamic t BaseUrl

  rec
    dUser <- holdDyn Nothing never
    let appState :: AppState t
        appState = AppState dNow url dUser (demux dRoute)

    dRoute <- makeHistoryRouter RouteHome eSetRoute
    ((), eSetRoute) <- flip runReaderT appState . runEventWriterT $
      topLevel . void . dyn . ffor dRoute $ \case
        RouteHome -> homePage
        RouteLogin -> loginRegisterPage
        RouteRegister -> loginRegisterPage
        RouteProfile x -> profilePage x
        RouteSettings -> settingsPage
        RouteEditor mx -> createEditArticlePage mx
        RouteArticle x -> articlePage x
  blank

topLevel :: AppStateM t m => m () -> m ()
topLevel contents =
  elAttr "div" ("id" =: "app") $ headerView >> contents >> footerView

headerView :: forall t m. AppStateM t m => m ()
headerView =
  elClass "nav" "navbar navbar-light" $ divClass "container" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "/index.html") (text "conduit")
    elClass "ul" "nav navbar-nav pull-xs-right" $ do
      sel <- asks stateRoute
      dMenuItems <- ffor (asks stateUser) $ fmap (\case
        Nothing ->
          [ (RouteHome, text "Home")
          , (RouteLogin, text "Sign in")
          , (RouteRegister, text "Sign up")
          ]
        Just u ->
          [ (RouteHome, text "home")
          , (RouteEditor Nothing, elClass "i" "ion-compose" blank >> nbsp >> text "New Post")
          , (RouteSettings, elClass "i" "ion-gear-a" blank >> nbsp >> text "Settings")
          , let un = u ^. field @"username" in (RouteProfile un, text un)
          ])
      void $ simpleList dMenuItems (menuLink sel)
  where
    menuLink :: Demux t Route -> Dynamic t (Route, m ()) -> m ()
    menuLink sel drc = void . dyn . ffor drc $ \(r, c) -> elClass "li" "nav-item" $
      appLink r (("class" =:) . bool "nav-link" "nav-link active" <$> demuxed sel r) (pure True) c

footerView :: MonadWidget t m => m ()
footerView =
  el "footer" $ divClass "container" $ do
    elAttr "a" ("class" =: "logo-font" <> "href" =: "/") (text "conduit")
    elClass "span" "attribution" $ do
      text "An interactive learning project from "
      elAttr "a" ("href" =: "https://thinkster.io") (text "Thinkster")
      text ". Code & design licensed under MIT."

-- List of articles pulled from either Feed, Global, or by Tag
-- Pagination for list of articles
homePage :: AppStateM t m => m ()
homePage = divClass "home-page" $ do
  pb <- getPostBuild
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

      api <- getApi
      eArticles <- fmap (^. field @"articles") . fmapMaybe reqSuccess <$> getArticles api
        (pure QNone) (pure QNone) (pure QNone) (pure QNone) (pure QNone) pb
      dArticles <- holdDyn [] eArticles
      void $ simpleList dArticles articlePreview

    divClass "col-md-3" $ divClass "sidebar" $ do
      el "p" (text "Popular Tags")
      elClass "div" "tag-list" $ do
        api <- getApi
        eTags <- fmap (^. field @"tags") . fmapMaybe reqSuccess <$> tagsGet api pb
        dTags <- holdDyn [] eTags
        void . simpleList dTags $ \t ->
          elAttr "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (dynText t)

articlePreview :: AppStateM t m => Dynamic t Article -> m ()
articlePreview dArticle =
  divClass "article-preview" $ do
    let dUsername = ffor dArticle (^. field @"author" . field @"username")
        dImage = ffor dArticle (^. field @"author" . field @"image")
        dSlug = ffor dArticle (^. field @"slug")
    divClass "article-meta" $ do
      appLinkDyn (RouteProfile <$> dUsername) mempty (pure True) $
        elDynAttr "img" ("src"  =! dImage) blank
      divClass "info" $ do
        appLinkDyn (RouteProfile <$> dUsername) (pure $ "class" =: "author") (pure True) (dynText dUsername)
        elClass "span" "date" (text "January 20th")
        elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
          elClass "i" "ion-heart" blank
          dynText $ ffor dArticle (tshow . (^. field @"favoritesCount"))
    appLinkDyn (RouteArticle <$> dSlug) (pure $ "class" =: "preview-link") (pure True) $ do
      el "h1" . dynText $ ffor dArticle (^. field @"title")
      el "p" . dynText $ ffor dArticle (^. field @"description")
      el "span" (text "Read more...")

-- Uses JWT (store the token in localStorage)
-- Authentication can be easily switched to session/cookie based
loginRegisterPage :: AppStateM t m => m ()
loginRegisterPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ do
    elClass "h1" "text-xs-center" (text "Sign up")
    elClass "p" "text-xs-center" $
      appLink RouteLogin (pure mempty) (pure True) (text "Have an account?")
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

-- Show basic user info
-- List of articles populated from author's created articles or author's favorited articles
profilePage :: MonadWidget t m => Text -> m ()
profilePage userSlug = divClass "profile-page" $ do
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

createEditArticlePage :: AppStateM t m => Maybe Text -> m ()
createEditArticlePage mArticleSlug =
  divClass "editor-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-10 offset-md-1 col-xs-12" $
      el "form" $ el "fieldset" $ do
    pb <- getPostBuild
    api <- getApi
    eInit <- case mArticleSlug of
      Nothing -> pure never
      Just a -> fmap (^. field @"article") . fmapMaybe reqSuccess <$> getArticle api (pure (Right a)) pb

    dTitle <- fmap value . elClass "fieldset" "form-group" $
      inputElement $ def
        & inputElementConfig_setValue .~ ffor eInit (^. field @"title")
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: "text" <> "class" =: "form-control form-control-lg" <> "placeholder" =: "Article Title")
    dDesc <- fmap value . elClass "fieldset" "form-group" $
      inputElement $ def
        & inputElementConfig_setValue .~ ffor eInit (^. field @"description")
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "What's this article about?")
    dBody <- fmap value . elClass "fieldset" "form-group" $
      textAreaElement $ def
        & textAreaElementConfig_setValue .~ ffor eInit (^. field @"body")
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("class" =: "form-control" <> "rows" =: "8" <> "placeholder" =: "Write your article (in markdown)")
    dTags <- elClass "fieldset" "form-group" $ do
      rec
        dTag <- inputElement $ def
          & inputElementConfig_setValue .~ ("" <$ keypress Enter dTag)
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Enter tags")
        tags <- foldDyn ($) [] $ leftmost
          [ ffor eInit (const . (^. field @"tagList"))
          , (:) <$> current (value dTag) <@ keypress Enter dTag
          , ffor eDelete (\x -> filter (/= x))
          ]
        eDelete <- fmap (switchDyn . fmap leftmost) . divClass "tag-list" $ simpleList tags $ \t ->
          elClass "span" "tag-default tag-pill" $ do
            (btn, ()) <- elClass' "i" "ion-close-round" blank
            dynText t
            pure $ current t <@ domEvent Click btn
      pure tags
    (btn, ()) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" (text "Publish Article")
    let dArticle = NewArticle <$> dTitle <*> dDesc <*> dBody <*> dTags
    eRes <- case mArticleSlug of
      Nothing -> createArticle api (Right . NewArticleRequest <$> dArticle) (domEvent Click btn)
      Just a -> updateArticle api (pure $ Right a) (Right . UpdateArticleRequest <$> dArticle) (domEvent Click btn)
    let (eErr, eRedirect) = fanEither . ffor eRes $ \case
          ResponseSuccess _ a _ -> Right (RouteArticle $ a ^. field @"article" . field @"slug")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent eRedirect
    dynText =<< holdDyn "" (leftmost ["" <$ domEvent Click btn, eErr])

-- Delete article button (only shown to article's author)
-- Render markdown from server client side
-- Comments section at bottom of page
-- Delete comment button (only shown to comment's author)
articlePage :: MonadWidget t m => Text -> m ()
articlePage articleSlug = divClass "article-page" $ do
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
