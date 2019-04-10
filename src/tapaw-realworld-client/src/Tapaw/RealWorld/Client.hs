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
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Bool (bool)
import Data.Char (chr)
import Data.Generics.Product (field)
import Data.Map (Map)
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
import Tapaw.RealWorld.Client.Utils ((=!), (<!>), tshow)


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
        RouteLogin -> loginPage
        RouteRegister -> registerPage
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
    appLink RouteHome (pure $ "class" =: "navbar-brand") (pure True) (text "conduit")
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

footerView :: AppStateM t m => m ()
footerView =
  el "footer" $ divClass "container" $ do
    appLink RouteHome (pure $ "class" =: "logo-font") (pure True) (text "conduit")
    elClass "span" "attribution" $ do
      text "An interactive learning project from "
      elAttr "a" ("href" =: "https://thinkster.io") (text "Thinkster")
      text ". Code & design licensed under MIT."

-- List of articles pulled from either Feed, Global, or by Tag
-- Pagination for list of articles
homePage :: AppStateM t m => m ()
homePage = divClass "home-page" $ do
  pb <- getPostBuild
  api <- getApi
  divClass "banner" $ divClass "container" $ do
    elClass "h1" "logo-font" (text "conduit")
    el "p" (text "A place to share your knowledge.")
  divClass "container page" $ divClass "row" $ mdo
    eArticles <- (^!. field @"articles") . fmapMaybe reqSuccess <$> getArticles api
      dTag (pure QNone) (pure QNone) (pure QNone) (pure QNone)
      (leftmost [pb, () <$ updated dTag])
    dArticles <- holdDyn [] eArticles
    eTags <- (^!. field @"tags") . fmapMaybe reqSuccess <$> tagsGet api pb
    dTags <- holdDyn [] eTags

    divClass "col-md-9" $ do
      divClass "feed-toggle" $ elClass "ul" "nav nav-pills outline-active" $ do
        elClass "li" "nav-item" $
          elAttr "a" ("class" =: "nav-link disabled" <> "href" =: "") (text "Your Feed")
        elClass "li" "nav-item" $
          elAttr "a" ("class" =: "nav-link active" <> "href" =: "") (text "Global Feed")
      void $ simpleList dArticles articlePreview

    eTag <- fmap (switchDyn . fmap leftmost) . divClass "col-md-3" $ divClass "sidebar" $ do
      el "p" (text "Popular Tags")
      elClass "div" "tag-list" . simpleList dTags $ \t ->
       (current t <@) . domEvent Click . fst <$>
       elAttr' "a" ("href" =: "" <> "class" =: "tag-pill tag-default") (dynText t)
    dTag <- holdDyn QNone (QParamSome . tmaybe <$> eTag)
    pure ()

articlePreview :: AppStateM t m => Dynamic t Article -> m ()
articlePreview dArticle =
  divClass "article-preview" $ do
    divClass "article-meta" (articleMeta dArticle False)
    appLinkDyn (RouteArticle . (^. field @"slug") <$> dArticle)
        (pure $ "class" =: "preview-link") (pure True) $ do
      el "h1" . dynText $ dArticle ^!. field @"title"
      el "p" . dynText $ dArticle ^!. field @"description"
      el "span" (text "Read more...")

-- Uses JWT (store the token in localStorage)
-- Authentication can be easily switched to session/cookie based
loginPage :: AppStateM t m => m ()
loginPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" (text "Sign in")
    elClass "p" "text-xs-center" $
      appLink RouteRegister (pure mempty) (pure True) (text "Need an account?")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr

    (form, (dReq, eClick)) <- el' "form" $ do
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "email" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      (btn, ()) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" (text "Sign up")
      pure (LoginUser <$> dEmail <*> dPassword, domEvent Click btn)
    api <- getApi
    eRes <- login api (Right . LoginUserRequest <$> dReq) (domEvent Submit form <> eClick)
    let (eErr, eUser) = fanEither . ffor eRes $ \case
          ResponseSuccess _ r _ -> Right (r ^. field @"user")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    -- TODO: store token
    tellEvent $ RouteHome <$ eUser
    pure ()

registerPage :: AppStateM t m => m ()
registerPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" (text "Sign up")
    elClass "p" "text-xs-center" $
      appLink RouteLogin (pure mempty) (pure True) (text "Have an account?")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr

    (form, (dReq, eClick)) <- el' "form" $ do
      dName <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Your Name")
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "email" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      (btn, ()) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" (text "Sign up")
      pure (NewUser <$> dName <*> dEmail <*> dPassword, domEvent Click btn)
    api <- getApi
    eRes <- createUser api (Right . NewUserRequest <$> dReq) (domEvent Submit form <> eClick)
    let (eErr, eUser) = fanEither . ffor eRes $ \case
          ResponseSuccess _ r _ -> Right (r ^. field @"user")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    -- TODO: store token
    tellEvent $ RouteHome <$ eUser

-- Show basic user info
-- List of articles populated from author's created articles or author's favorited articles
profilePage :: AppStateM t m => Text -> m ()
profilePage userSlug = divClass "profile-page" $ do
  pb <- getPostBuild
  api <- getApi
  eUser <- fmapMaybe reqSuccess <$> getProfileByUsername api (pure $ Right userSlug) pb
  dUser <- holdDyn (Profile "" "" "" False) (eUser ^!. field @"profile")

  divClass "user-info" $ divClass "container" $
      divClass "row" $ divClass "col-xs-12 col-md-10 offset-md-1" $ do
    elDynAttr "img" ("class" =: "user-img" <!> "src" =! (dUser ^!. field @"image")) blank
    el "h4" . dynText $ dUser ^!. field @"username"
    el "p" . dynText $ dUser ^!. field @"bio"
    elClass "button" "btn btn-sm btn-outline-secondary action-btn" $ do
      elClass "i" "ion-plus-round" blank
      nbsp
      text "Follow Eric Simons"

  eFeed <- getArticles api (pure QNone) (pure . QParamSome $ Just userSlug) (pure QNone) (pure QNone) (pure QNone) pb
  dFeed <- holdDyn [] $ (^. field @"articles") <$> fmapMaybe reqSuccess eFeed

  divClass "container" $ divClass "row" $
    divClass "col-xs-12 col-md-10 offset-md-1" $ do
      divClass "articles-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $
            elAttr "a" ("class" =: "nav-link active" <> "href" =: "") (text "My Articles")
          elClass "li" "nav-item" $
            elAttr "a" ("class" =: "nav-link" <> "href" =: "") (text "Favorited Articles")

      void . simpleList dFeed $ \dArticle ->
        divClass "articles-preview" $ do
          divClass "article-meta" (articleMeta dArticle False)
          appLinkDyn (RouteArticle . (^. field @"slug") <$> dArticle) (pure $ "class" =: "preview-link") (pure True) $ do
            el "h1" . dynText $ dArticle ^!. field @"title"
            el "p" . dynText $ dArticle ^!. field @"description"
            el "span" $ text "Read more..."
            void . dyn . ffor dArticle $ \x -> case x ^. field @"tagList" of
              [] -> pure ()
              xs -> elClass "ul" "tag-list" . forM_ xs $
                elClass "li" "tag-default tag-pill tag-outline" . text

articleMeta :: AppStateM t m => Dynamic t Article -> Bool -> m ()
articleMeta dArticle showFull = do
  let dProfile = dArticle ^!. field @"author"
  appLinkDyn (RouteProfile . (^. field @"username") <$> dProfile) mempty (pure True) $
    elDynAttr "img" ("src" =! ffor dProfile (^. field @"image")) blank
  divClass "info" $ do
    appLinkDyn (RouteProfile . (^. field @"username") <$> dProfile) (pure $ "class" =: "author") (pure True) $
      dynText (dProfile ^!. field @"username")
    elClass "span" "date" (text "January 20th")
  if showFull
    then do
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-plus-round" blank
        nbsp
        text "Follow Eric Simons"
      nbsp >> nbsp
      elClass "button" "btn btn-sm btn-outline-secondary" $ do
        elClass "i" "ion-heart" blank
        nbsp
        text "Favorite Post "
        elClass "span" "counter" . dynText . ffor dArticle $ tshow . (^. field @"favoritesCount")
    else
      elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
        elClass "i" "ion-heart" blank
        elClass "span" "counter" . dynText . ffor dArticle $ tshow . (^. field @"favoritesCount")

settingsPage :: AppStateM t m => m ()
settingsPage =
  divClass "settings-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    pb <- getPostBuild
    api <- getApi
    eInit <- (^!. field @"user") . fmapMaybe reqSuccess <$> getCurrentUser api pb

    elClass "h1" "text-xs-center" (text "Your Settings")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr
    (form, (dReq, eClick)) <- el' "form" $ el "fieldset" $ do
      dImage <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"image")
        ("class" =: "form-control" <> "placeholder" =: "URL of profile picture")
      dName <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"username")
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Your Name")
      dBio <- fmap value . elClass "fieldset" "form-group" $ inputTextarea (eInit ^!. field @"bio")
        ("class" =: "form-control form-control-lg" <> "rows" =: "8" <> "placeholder" =: "Short bio about you")
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"email")
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"email")
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      (btn, ()) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" (text "Update Settings")
      pure (UpdateUser <$> dName <*> dEmail <*> dBio <*> dImage <*> fmap tmaybe dPassword, domEvent Click btn)
    eRes <- updateCurrentUser api (Right . UpdateUserRequest <$> dReq) (domEvent Submit form <> eClick)
    let (eErr, eUser) = fanEither . ffor eRes $ \case
          ResponseSuccess{} -> Right ()
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent $ RouteHome <$ eUser
    -- TODO: Logout button

createEditArticlePage :: AppStateM t m => Maybe Text -> m ()
createEditArticlePage mArticleSlug =
  divClass "editor-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-10 offset-md-1 col-xs-12" $
      el "form" $ el "fieldset" $ do
    pb <- getPostBuild
    api <- getApi
    eInit <- case mArticleSlug of
      Nothing -> pure never
      Just a -> (^!. field @"article") . fmapMaybe reqSuccess <$> getArticle api (pure (Right a)) pb

    dTitle <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"title")
      ("class" =: "form-control form-control-lg" <> "placeholder" =: "Article Title")
    dDesc <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"description")
      ("class" =: "form-control" <> "placeholder" =: "What's this article about?")
    dBody <- fmap value . elClass "fieldset" "form-group" $ inputTextarea (eInit ^!. field @"body")
      ("class" =: "form-control" <> "rows" =: "8" <> "placeholder" =: "Write your article (in markdown)")
    dTags <- elClass "fieldset" "form-group" $ mdo
      dTag <- inputText ("" <$ keypress Enter dTag) ("class" =: "form-control" <> "placeholder" =: "Enter tags")
      tags <- foldDyn ($) [] $ leftmost
        [ const <$> eInit ^!. field @"tagList"
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
articlePage :: AppStateM t m => Text -> m ()
articlePage articleSlug = divClass "article-page" $ do
  pb <- getPostBuild
  api <- getApi
  eArticle <- fmapMaybe reqSuccess <$> getArticle api (pure $ Right articleSlug) pb
  dArticle <- holdDyn (Article "" "" "" "" [] 0 0 False 0 (Profile "" "" "" False))
    (eArticle ^!. field @"article")
  divClass "banner" $ divClass "container" $ do
    el "h1" . dynText $ ffor dArticle (^. field @"title")
    divClass "article-meta" (articleMeta dArticle True)
  divClass "container page" $ do
    divClass "row article-content" $ divClass "col-md-12" $
      el "p" . dynText $ dArticle ^!. field @"body"

    el "hr" blank
    divClass "article-actions" $ divClass "article-meta" (articleMeta dArticle True)

    divClass "row" $ divClass "col-xs-12 col-md-8 offset-md-2" $ do
      rec
        (form, (dReqComment, eClick)) <- elClass' "form" "card comment-form" $ do
          dText <- fmap value . divClass "card-block" $ inputTextarea ("" <$ eResComment)
            ("class" =: "form-control" <> "placeholder" =: "Write a comment..." <> "rows" =: "3")
          (btn, ()) <- divClass "card-footer" $ do
            elDynAttr "img" ("class" =: "comment-author-tag" <!> "src" =! dArticle ^!. field @"author" . field @"image") blank
            elClass' "button" "btn btn-sm btn-primary" (text "Post Comment")
          pure (NewComment <$> dText, domEvent Click btn)
        eResComment <- fmapMaybe reqSuccess <$> createArticleComment api
          (pure $ Right articleSlug)
          (Right . NewCommentRequest <$> dReqComment)
          (domEvent Submit form <> eClick)

      eComments <- fmapMaybe reqSuccess <$> getArticleComments api
        (Right . (^. field @"slug") <$> dArticle) pb
      dComments <- foldDyn ($) [] $ leftmost
        [const . (^. field @"comments") <$> eComments
        , (:) . (^. field @"comment") <$> eResComment
        ]

      void . simpleList dComments $ \dComment -> divClass "card" $ do
        let dUsername = dComment ^!. field @"author" . field @"username"
            dImage = dComment ^!. field @"author" . field @"image"
        divClass "card-block" $
          elClass "p" "card-text" . dynText $ dComment ^!. field @"body"
        divClass "card-footer" $ do
          appLinkDyn (RouteProfile <$> dUsername) (pure $ "class" =: "comment-author") (pure True) $
            elDynAttr "img" ("class" =: "comment-author-img" <!> "src" =! dImage) blank
          nbsp
          appLinkDyn (RouteProfile <$> dUsername) (pure $ "class" =: "comment-author") (pure True) $
            dynText dUsername
          elClass "span" "date-posted" (text "Dec 29th")
          -- TODO: only for comment author
          elClass "span" "mod-options" $ elClass "i" "ion-trash-a" blank

nbsp :: DomBuilder t m => m ()
nbsp = text . T.singleton $ chr 160

tmaybe :: Text -> Maybe Text
tmaybe "" = Nothing
tmaybe x = Just x

inputText ::
     MonadWidget t m
  => Event t Text
  -> Map AttributeName Text
  -> m (InputElement EventResult (DomBuilderSpace m) t)
inputText eSet attrs =
  inputElement $ def
    & inputElementConfig_setValue .~ eSet
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs

inputTextarea ::
     MonadWidget t m
  => Event t Text
  -> Map AttributeName Text
  -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
inputTextarea eSet attrs =
  textAreaElement $ def
    & textAreaElementConfig_setValue .~ eSet
    & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs

infixl 8 ^!.
x ^!. f = fmap (^. f) x
