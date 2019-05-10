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
{-# LANGUAGE ViewPatterns #-}

module Tapaw.RealWorld.Client
  ( frontend
  ) where

import Control.Lens (Getting, (^.), (^?), (%~), _Just, _Right, re)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Data.Bool (bool)
import Data.Char (chr)
import Data.Generics.Product (field)
import Data.Map (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.These (These(..))
import Data.Time (getCurrentTime)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Storage as DOM
import qualified GHCJS.DOM.Window as DOM hiding (focus)
import GHCJS.DOM.Types (JSString, MonadJSM, liftJSM)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core hiding (Client, link)
import Servant.Reflex (BaseUrl(..), QParam(..), ReqResult(..), Scheme(..), reqSuccess)
import Tapaw.RealWorld.Types
import Tapaw.RealWorld.Client.API
import Tapaw.RealWorld.Client.Navigation (makeHistoryRouter, link, appLink, appLinkDyn)
import Tapaw.RealWorld.Client.Types
import Tapaw.RealWorld.Client.Utils ((=!), form, formAttr, tshow)


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

saveUserToken :: MonadJSM m => DOM.Storage -> Text -> m ()
saveUserToken st = DOM.setItem st ("userToken" :: JSString)

readUserToken :: MonadJSM m => DOM.Storage -> m (Maybe Text)
readUserToken st = DOM.getItem st ("userToken" :: JSString)

app :: forall t m. MonadWidget t m => m ()
app = do
  window <- DOM.currentWindowUnchecked
  storage <- DOM.getLocalStorage window
  t0 <- liftIO getCurrentTime
  dNow <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0
  let url = constDyn (BaseFullUrl Http "localhost" 3000 "/api") :: Dynamic t BaseUrl

  userToken <- liftJSM (readUserToken storage)
  rec
    let appState :: AppState t
        appState = AppState dNow url dUser (demux dRoute)

    dUser <- holdDyn Nothing eSetUser
    dRoute <- makeHistoryRouter RouteHome eSetRoute
    let (eSetRoute, eSetUser) = fanThese eEW
    ((), eEW) <- flip runReaderT appState . runEventWriterT $ do
      eUser0 <- getCurrentUser (pure $ maybe (Left "no token") Right userToken)  =<< getPostBuild
      tellEvent $ That <$> fmapMaybe reqSuccess eUser0 ^!. field @"user" . re _Just
      topLevel . void . dyn . ffor dRoute $ \case
        RouteHome -> homePage
        RouteLogin -> loginPage
        RouteRegister -> registerPage
        RouteProfile x -> profilePage x
        RouteProfileFavorites x -> profileFavoritesPage x
        RouteSettings -> settingsPage
        RouteEditor mx -> createEditArticlePage mx
        RouteArticle x -> articlePage x
  performEvent_ $ saveUserToken storage <$> fmapMaybe id (updated dUser) ^!. field @"token"

topLevel :: AppStateM t m => m () -> m ()
topLevel contents =
  elAttr "div" ("id" =: "app") $ headerView >> contents >> footerView

headerView :: forall t m. AppStateM t m => m ()
headerView =
  elClass "nav" "navbar navbar-light" $ divClass "container" $ do
    appLink RouteHome (pure $ "class" =: "navbar-brand") (pure True) (text "conduit")
    elClass "ul" "nav navbar-nav pull-xs-right" $ do
      sel <- asks stateRoute
      dMenuItems <- fmap (maybe anonymRoutes userRoutes) <$> asks stateUser
      void $ simpleList dMenuItems (menuLink sel)
  where
    menuLink :: Demux t Route -> Dynamic t (Route, m ()) -> m ()
    menuLink sel drc = void . dyn . ffor drc $ \(r, c) -> elClass "li" "nav-item" $
      appLink r (("class" =:) . bool "nav-link" "nav-link active" <$> demuxed sel r) (pure True) c

    anonymRoutes :: [(Route, m ())]
    anonymRoutes =
      [ (RouteHome, text "Home")
      , (RouteLogin, text "Sign in")
      , (RouteRegister, text "Sign up")
      ]
    userRoutes u =
      [ (RouteHome, text "Home")
      , (RouteEditor Nothing, elClass "i" "ion-compose" blank >> nbsp >> text "New Post")
      , (RouteSettings, elClass "i" "ion-gear-a" blank >> nbsp >> text "Settings")
      , let un = u ^. field @"username" in (RouteProfile un, text ("@" <> un))
      ]

footerView :: AppStateM t m => m ()
footerView =
  el "footer" $ divClass "container" $ do
    appLink RouteHome (pure $ "class" =: "logo-font") (pure True) (text "conduit")
    elClass "span" "attribution" $ do
      text "An interactive learning project from "
      elAttr "a" ("href" =: "https://thinkster.io") (text "Thinkster")
      text ". Code & design licensed under MIT."

data FeedSource
  = FeedGlobal
  | FeedMy
  | FeedTag Text
  deriving (Eq, Ord, Show)

asToken :: Maybe User -> Either Text Text
asToken = maybe (Left "no token") (\u -> Right $ u ^. field @"token")

homePage :: forall t m. AppStateM t m => m ()
homePage = divClass "home-page" $ do
  divClass "banner" $ divClass "container" $ do
    elClass "h1" "logo-font" (text "conduit")
    el "p" (text "A place to share your knowledge.")
  divClass "container page" $ divClass "row" $ mdo
    dFeedSource <- (holdUniqDyn =<<) . holdDyn FeedGlobal $ leftmost
      [maybe FeedGlobal (const FeedMy) <$> updated dmUser, eSel, FeedTag <$> eSelTag]
    pb <- getPostBuild
    dmUser <- asks stateUser
    eArticles <- switchDyn <$> widgetHold (pure never) (getArticleSource dmUser <$> updated dFeedSource)
    dArticles <- holdDyn [] (fmapMaybe reqSuccess eArticles ^!. field @"articles")
    dTags <- holdDyn [] =<< (^!. field @"tags") . fmapMaybe reqSuccess <$> tagsGet pb

    eSel <- divClass "col-md-9" $ do
      dMenu <- holdUniqDyn . ffor2 dmUser dFeedSource $ \a b -> case (a, b) of
        (Nothing, FeedTag t) -> [(FeedGlobal, "Global Feed"), (FeedTag t, "#"<>t)]
        (Nothing, _) -> [(FeedGlobal, "Global Feed")]
        (Just _, FeedTag t) -> [(FeedMy, "Your Feed"), (FeedGlobal, "Global Feed"), (FeedTag t, "#"<>t)]
        (Just _, _) -> [(FeedMy, "Your Feed"), (FeedGlobal, "Global Feed")]
      eSel' <- divClass "feed-toggle" . elClass "ul" "nav nav-pills outline-active" .
        fmap (switchDyn . fmap leftmost) . simpleList dMenu $ menuLink dFeedSource
      void $ simpleList dArticles articlePreview
      pure eSel'

    eSelTag <- fmap (switchDyn . fmap leftmost) . divClass "col-md-3" $ divClass "sidebar" $ do
      el "p" (text "Popular Tags")
      elClass "div" "tag-list" . simpleList dTags $ \t -> do
        eClick <- link (pure $ "href" =: "" <> "class" =: "tag-pill tag-default") (pure True) (dynText t)
        pure (current t <@ eClick)
    pure ()
  where
    getArticleSource dUser x = case x of
      FeedGlobal -> getArticles (pure QNone) (pure QNone) (pure QNone) (pure QNone) (pure QNone) =<< getPostBuild
      FeedMy -> getArticlesFeed (asToken <$> dUser) (pure QNone) (pure QNone) =<< getPostBuild
      FeedTag t -> getArticles (pure $ QParamSome t) (pure QNone) (pure QNone) (pure QNone) (pure QNone) =<< getPostBuild
    menuLink dFeedSource (splitDynPure -> (dTgt, dTxt)) = do
      eClick <- elClass "li" "nav-item" $ link
        (("href" =: "" <>) . ("class" =:) . ("nav-link " <>) . bool "" "active" <$>
         ffor2 dTgt dFeedSource (==))
        (pure True) (dynText dTxt)
      pure $ current dTgt <@ eClick

articlePreview :: AppStateM t m => Dynamic t Article -> m ()
articlePreview dArticle =
  divClass "article-preview" $ do
    divClass "article-meta" (articleMeta dArticle False)
    appLinkDyn (RouteArticle . (^. field @"slug") <$> dArticle)
        (pure $ "class" =: "preview-link") (pure True) $ do
      el "h1" . dynText $ dArticle ^!. field @"title"
      el "p" . dynText $ dArticle ^!. field @"description"
      el "span" (text "Read more...")
      void . dyn . ffor dArticle $ \x -> case x ^. field @"tagList" of
        [] -> pure ()
        xs -> elClass "ul" "tag-list" . forM_ xs $
          elClass "li" "tag-default tag-pill tag-outline" . text

loginPage :: AppStateM t m => m ()
loginPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" (text "Sign in")
    elClass "p" "text-xs-center" $
      appLink RouteRegister (pure mempty) (pure True) (text "Need an account?")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr

    (eSubmit, dReq) <- form $ do
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "email" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      elAttr "button" ("type" =: "submit" <> "class" =: "btn btn-lg btn-primary pull-xs-right") (text "Sign up")
      pure (LoginUser <$> dEmail <*> dPassword)
    eRes <- login (Right . LoginUserRequest <$> dReq) eSubmit
    let (eErr, eUser) = fanEither . ffor eRes $ \case
          ResponseSuccess _ r _ -> Right (r ^. field @"user")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent $ These RouteHome . Just <$> eUser

registerPage :: AppStateM t m => m ()
registerPage =
  divClass "auth-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" (text "Sign up")
    elClass "p" "text-xs-center" $
      appLink RouteLogin (pure mempty) (pure True) (text "Have an account?")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr

    (eSubmit, dReq) <- form $ do
      dName <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Your Name")
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "email" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText never
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      elAttr "button" ("type" =: "submit" <> "class" =: "btn btn-lg btn-primary pull-xs-right") (text "Sign up")
      pure (NewUser <$> dName <*> dEmail <*> dPassword <*> pure "" <*> pure "")
    eRes <- createUser (Right . NewUserRequest <$> dReq) eSubmit
    let (eErr, eUser) = fanEither . ffor eRes $ \case
          ResponseSuccess _ r _ -> Right (r ^. field @"user")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent $ These RouteHome . Just <$> eUser

profileHeader :: AppStateM t m => Text -> m ()
profileHeader userSlug = mdo
  pb <- getPostBuild
  eProfile <- fmapMaybe reqSuccess <$> getProfileByUsername (pure $ Right userSlug) pb
  dProfile <- holdDyn (Profile "" "" "" False) (eProfile ^!. field @"profile")
  dCurrent <- asks stateUser

  divClass "user-info" . divClass "container" . divClass "row" .
      divClass "col-xs-12 col-md-10 offset-md-1" $ do
    profileImg "user-img" dProfile
    el "h4" . dynText $ dProfile ^!. field @"username"
    el "p" . dynText $ dProfile ^!. field @"bio"
    void . dyn . ffor2 dCurrent dProfile $ \cur auth ->
      when (cur ^? _Just . field @"username" /= auth ^. field @"username" . re _Just) 
        (followButton dProfile)

profilePage :: AppStateM t m => Text -> m ()
profilePage (T.dropWhile (== '@') -> userSlug) = divClass "profile-page" $ do
  profileHeader userSlug

  pb <- getPostBuild
  eFeed <- getArticles (pure QNone) (pure $ QParamSome userSlug) (pure QNone)
    (pure QNone) (pure QNone) pb
  dFeed <- holdDyn [] $ (^. field @"articles") <$> fmapMaybe reqSuccess eFeed

  divClass "container" $ divClass "row" $
    divClass "col-xs-12 col-md-10 offset-md-1" $ do
      divClass "articles-toggle" . elClass "ul" "nav nav-pills outline-active" $ do
        elClass "li" "nav-item" $
          appLink (RouteProfile userSlug) (pure $ "class" =: "nav-link active")
            (pure False) (text "My Articles")
        elClass "li" "nav-item" $
          appLink (RouteProfileFavorites userSlug) (pure $ "class" =: "nav-link")
            (pure True) (text "Favorited Articles")
      void $ simpleList dFeed articlePreview

profileFavoritesPage :: AppStateM t m => Text -> m ()
profileFavoritesPage (T.dropWhile (== '@') -> userSlug) = divClass "profile-page" $ do
  profileHeader userSlug

  pb <- getPostBuild
  eFeed <- getArticles (pure QNone) (pure QNone) (pure $ QParamSome userSlug)
    (pure QNone) (pure QNone) pb
  dFeed <- holdDyn [] $ (^. field @"articles") <$> fmapMaybe reqSuccess eFeed

  divClass "container" $ divClass "row" $
    divClass "col-xs-12 col-md-10 offset-md-1" $ do
      divClass "articles-toggle" . elClass "ul" "nav nav-pills outline-active" $ do
        elClass "li" "nav-item" $
          appLink (RouteProfile userSlug) (pure $ "class" =: "nav-link")
            (pure True) (text "My Articles")
        elClass "li" "nav-item" $
          appLink (RouteProfileFavorites userSlug) (pure $ "class" =: "nav-link active")
            (pure False) (text "Favorited Articles")
      void $ simpleList dFeed articlePreview

followButton :: AppStateM t m => Dynamic t Profile -> m ()
followButton dProfile' = mdo
  dmUser <- asks stateUser
  profile_ <- sample (current dProfile')
  dProfile <- holdDyn profile_ $ leftmost
    [updated dProfile', fmapMaybe reqSuccess eRes ^!. field @"profile"]
  eRes' <- dyn $ ffor f $ \f' -> f' (asToken <$> dmUser) (dProfile ^!. field @"username" . re _Right) (domEvent Click btn)
  eRes <- switchHold never eRes'
  let follow = dProfile ^!. field @"following"
      cls = bool " btn-outline-secondary" " btn-secondary" <$> follow
      btnText = bool "Follow " "Unfollow " <$> follow
      icon = bool "ion-plus-round" "ion-minus-round" <$> follow
      f = bool followUserByUsername unfollowUserByUsername <$> follow
  (btn, ()) <- elDynAttr' "button" (("class" =:) . ("btn btn-sm action-btn" <>) <$> cls) $ do
    elDynAttr "i" ("class" =! icon) blank
    nbsp >> dynText btnText
    dynText $ dProfile ^!. field @"username"
  pure ()

heartButton :: AppStateM t m => Dynamic t Article -> m ()
heartButton dArticle' = mdo
  dmUser <- asks stateUser
  article_ <- sample (current dArticle')
  dArticle <- holdDyn article_ $ leftmost
    [updated dArticle', fmapMaybe reqSuccess eRes ^!. field @"article"]
  eRes' <- dyn $ ffor f $ \f' -> f' (asToken <$> dmUser) (dArticle ^!. field @"slug" . re _Right) (domEvent Click btn)
  eRes <- switchHold never eRes'

  let heart = dArticle ^!. field @"favorited"
      cls = bool "btn-outline-secondary" "btn-secondary" <$> heart
      btnText = bool "Favorite Post " "Unfavorite Post " <$> heart
      f = bool createArticleFavorite deleteArticleFavorite <$> heart

  (btn, ()) <- elDynAttr' "button" (("class" =:) . ("btn btn-sm " <>) <$> cls) $ do
    elClass "i" "ion-heart" blank >> nbsp >> dynText btnText
    elClass "span" "counter" $
      text "(" >> dynText (tshow . (^. field @"favoritesCount") <$> dArticle) >> text ")"
  pure ()

heartButtonSmall :: AppStateM t m => Dynamic t Article -> m ()
heartButtonSmall dArticle' = mdo
  dmUser <- asks stateUser
  article_ <- sample (current dArticle')
  dArticle <- holdDyn article_ $ leftmost
    [updated dArticle', fmapMaybe reqSuccess eRes ^!. field @"article"]
  eRes' <- dyn $ ffor f $ \f' -> f' (asToken <$> dmUser) (dArticle ^!. field @"slug" . re _Right) (domEvent Click btn)
  eRes <- switchHold never eRes'
  let heart = dArticle ^!. field @"favorited"
      cls = bool "btn-outline-primary" "btn-primary" <$> heart
      f = bool createArticleFavorite deleteArticleFavorite <$> heart
  (btn, ()) <- elDynAttr' "button" (("class" =:) . ("btn pull-xs-right btn-sm " <>) <$> cls) $ do
    elClass "i" "ion-heart" blank >> nbsp
    elClass "span" "counter" $
      text "(" >> dynText (tshow . (^. field @"favoritesCount") <$> dArticle) >> text ")"
  pure ()

articleMeta :: AppStateM t m => Dynamic t Article -> Bool -> m ()
articleMeta dArticle showFull = do
  let dProfile = dArticle ^!. field @"author"
  profileLink "" dProfile (profileImg "" dProfile)
  divClass "info" $ do
    profileLink "author" dProfile $ dynText (dProfile ^!. field @"username")
    elClass "span" "date" (text "January 20th")
  if showFull
    then followButton dProfile >> nbsp >> nbsp >> heartButton dArticle
    else heartButtonSmall dArticle

settingsPage :: AppStateM t m => m ()
settingsPage =
  divClass "settings-page" $ divClass "container page" $
      divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    pb <- getPostBuild
    dmUser <- asks stateUser
    let eUser = fmapMaybe id $ leftmost [current dmUser <@ pb, updated dmUser]

    elClass "h1" "text-xs-center" (text "Your Settings")
    elClass "ul" "error-messages" . el "li" . dynText =<< holdDyn "" eErr
    (eSubmit, dReq) <- form $ el "fieldset" $ do
      dImage <- fmap value . elClass "fieldset" "form-group" $ inputText (eUser ^!. field @"image" . _Just)
        ("class" =: "form-control" <> "placeholder" =: "URL of profile picture")
      dName <- fmap value . elClass "fieldset" "form-group" $ inputText (eUser ^!. field @"username")
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Your Name")
      dBio <- fmap value . elClass "fieldset" "form-group" $ inputTextarea (eUser ^!. field @"bio" . _Just)
        ("class" =: "form-control form-control-lg" <> "rows" =: "8" <> "placeholder" =: "Short bio about you")
      dEmail <- fmap value . elClass "fieldset" "form-group" $ inputText (eUser ^!. field @"email")
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Email")
      dPassword <- fmap value . elClass "fieldset" "form-group" $ inputText (eUser ^!. field @"email")
        ("class" =: "form-control form-control-lg" <> "type" =: "password" <> "placeholder" =: "Password")
      elAttr "button" ("type" =: "submit" <> "class" =: "btn btn-lg btn-primary pull-xs-right") (text "Update Settings")
      pure (UpdateUser <$> dName <*> dEmail <*> dBio <*> dImage <*> fmap tmaybe dPassword)
    eRes <- updateCurrentUser (asToken <$> dmUser) (Right . UpdateUserRequest <$> dReq) eSubmit
    let (eErr, eUpdated) = fanEither . ffor eRes $ \case
          ResponseSuccess _ a _ -> Right (a ^. field @"user")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent $ These RouteHome . Just <$> eUpdated

    el "hr" blank
    (logoutBtn, ()) <- elClass' "button" "btn btn-outline-danger" (text "Or click here to log out.")
    tellEvent $ These RouteHome Nothing <$ domEvent Click logoutBtn

createEditArticlePage :: AppStateM t m => Maybe Text -> m ()
createEditArticlePage mArticleSlug =
  divClass "editor-page" $ divClass "container page" $ divClass "row" $ divClass "col-md-10 offset-md-1 col-xs-12" $ do
    pb <- getPostBuild
    dmUser <- asks stateUser
    eInit <- case mArticleSlug of
      Nothing -> pure never
      Just a -> (^!. field @"article") . fmapMaybe reqSuccess <$> getArticle (pure (Right a)) pb

    (eSubmit, dArticle) <- form $ el "fieldset" $ do
      dTitle <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"title")
        ("class" =: "form-control form-control-lg" <> "placeholder" =: "Article Title")
      dDesc <- fmap value . elClass "fieldset" "form-group" $ inputText (eInit ^!. field @"description")
        ("class" =: "form-control" <> "placeholder" =: "What's this article about?")
      dBody <- fmap value . elClass "fieldset" "form-group" $ inputTextarea (eInit ^!. field @"body")
        ("class" =: "form-control" <> "rows" =: "8" <> "placeholder" =: "Write your article (in markdown)")
      dTags <- elClass "fieldset" "form-group" (tagInput $ eInit ^!. field @"tagList")
      elAttr "button" ("type" =: "submit" <> "class" =: "btn btn-lg btn-primary pull-xs-right") (text "Publish Article")
      pure (NewArticle <$> dTitle <*> dDesc <*> dBody <*> dTags)
    eRes <- case mArticleSlug of
      Nothing -> createArticle (asToken <$> dmUser) (Right . NewArticleRequest <$> dArticle) eSubmit
      Just a -> updateArticle (asToken <$> dmUser) (pure $ Right a) (Right . UpdateArticleRequest <$> dArticle) eSubmit
    let (eErr, eRedirect) = fanEither . ffor eRes $ \case
          ResponseSuccess _ a _ -> Right (RouteArticle $ a ^. field @"article" . field @"slug")
          ResponseFailure _ e _ -> Left e
          RequestFailure _ e -> Left e
    tellEvent (This <$> eRedirect)
    dynText =<< holdDyn "" (leftmost ["" <$ eSubmit, eErr])

tagInput :: forall t m. MonadWidget t m => Event t [Text] -> m (Dynamic t [Text])
tagInput eInit = mdo
  dTag <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_setValue .~ ("" <$ keypress Enter dTag)
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "form-control" <> "placeholder" =: "Enter tags")
    & inputElementConfig_elementConfig . elementConfig_eventSpec %~
        addEventSpecFlags (Proxy @(DomBuilderSpace m)) Keypress
        (\n -> if (keyCodeLookup . fromIntegral . unEventResult <$> n) == Just Enter then preventDefault else mempty)
  tags <- foldDyn ($) [] $ leftmost
    [ const <$> eInit
    , (:) <$> current (value dTag) <@ keypress Enter dTag
    , ffor eDelete (\x -> filter (/= x))
    ]
  eDelete <- fmap (switchDyn . fmap leftmost) . divClass "tag-list" $ simpleList tags $ \t ->
    elClass "span" "tag-default tag-pill" $ do
      (btn, ()) <- elClass' "i" "ion-close-round" blank
      dynText t
      pure $ current t <@ domEvent Click btn
  pure tags

articlePage :: AppStateM t m => Text -> m ()
articlePage articleSlug = divClass "article-page" $ do
  pb <- getPostBuild
  dmUser <- asks stateUser
  eArticle <- fmapMaybe reqSuccess <$> getArticle (pure $ Right articleSlug) pb
  dArticle <- holdDyn (Article "" "" "" "" [] "" "" False 0 (Profile "" "" "" False))
    (eArticle ^!. field @"article")

  divClass "banner" $ divClass "container" $ do
    el "h1" . dynText $ ffor dArticle (^. field @"title")
    divClass "article-meta" (articleMeta dArticle True)
  divClass "container page" $ do
    divClass "row article-content" $ divClass "col-md-12" $
      el "p" . dynText $ dArticle ^!. field @"body"

    el "hr" blank
    divClass "article-actions" $ divClass "article-meta" (articleMeta dArticle True)

    divClass "row" $ divClass "col-xs-12 col-md-8 offset-md-2" $ mdo
      eComments <- fmapMaybe reqSuccess <$> getArticleComments (pure . Right $ articleSlug) pb
      dComments <- foldDyn ($) [] $ leftmost
        [const . (^. field @"comments") <$> eComments
        , (:) . (^. field @"comment") <$> eResComment
        , (\x -> filter (\c -> c ^. field @"id" /= x)) <$> eDelete
        ]

      eResComment <- commentForm dmUser articleSlug
      eDelete <- switchDyn . fmap leftmost <$> simpleList dComments (commentView dmUser articleSlug)
      pure ()

commentView ::
     AppStateM t m
  => Dynamic t (Maybe User)
  -> Text
  -> Dynamic t Comment
  -> m (Event t Int)
commentView dmUser articleSlug dComment =
  divClass "card" $ do
    let dUsername = dComment ^!. field @"author" . field @"username"
        dAuthor = dComment ^!. field @"author"
    divClass "card-block" $
      elClass "p" "card-text" . dynText $ dComment ^!. field @"body"
    eDelete <- divClass "card-footer" $ do
      profileLink "comment-author" dAuthor $ profileImg "comment-author-img" dAuthor
      nbsp
      profileLink "comment-author" dAuthor $ dynText dUsername
      elClass "span" "date-posted" (text "Dec 29th")
      deleteButton dmUser dComment
    let dId = dComment ^!. field @"id"
    eResDelete <- deleteArticleComment (asToken <$> dmUser) (pure $ Right articleSlug) (Right <$> dId) eDelete
    pure $ current dId <@ fmapMaybe reqSuccess eResDelete

commentForm ::
     AppStateM t m
  => Dynamic t (Maybe User)
  -> Text
  -> m (Event t SingleCommentResponse)
commentForm dmUser articleSlug = (switchHold never =<<) . dyn . ffor dmUser $ \case
  Nothing -> do
    appLink RouteLogin (pure mempty) (pure True) (text "Sign in")
    text " or "
    appLink RouteRegister (pure mempty) (pure True) (text "Sign up")
    text " to add comments on this article."
    pure never
  Just u -> mdo
    (eSubmit, dReqComment) <- formAttr ("class" =: "card comment-form") $ do
      dText <- fmap value . divClass "card-block" $ inputTextarea ("" <$ eResComment)
        ("class" =: "form-control" <> "placeholder" =: "Write a comment..." <> "rows" =: "3")
      divClass "card-footer" $ do
        profileImg "comment-author-img"
          (pure $ Profile (u ^. field @"username") "" (u ^. field @"image" . _Just) False)
        elAttr "button" ("type" =: "submit" <> "class" =: "btn btn-sm btn-primary") (text "Post Comment")
      pure (NewComment <$> dText)
    eResComment <- fmapMaybe reqSuccess <$> createArticleComment
      (pure $ asToken (Just u)) (pure $ Right articleSlug)
      (Right . NewCommentRequest <$> dReqComment) eSubmit
    pure eResComment

deleteButton ::
     AppStateM t m
  => Dynamic t (Maybe User)
  -> Dynamic t Comment
  -> m (Event t Int)
deleteButton dmUser dComment =
  (switchHold never =<<) . dyn . ffor2 dmUser dComment $ \a b -> case (a, b) of
    (Just u, c) | u ^. field @"username" == c ^. field @"author" . field @"username" -> do
      (btn, ()) <- elClass' "span" "mod-options" $ elClass "i" "ion-trash-a" blank
      pure (current dComment ^!. field @"id" <@ domEvent Click btn)
    _ -> pure never

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

profileLink :: AppStateM t m => Text -> Dynamic t Profile -> m () -> m ()
profileLink cls dProfile =
  appLinkDyn (RouteProfile . (^. field @"username") <$> dProfile) (pure $ "class" =: cls) (pure True)

profileImg :: MonadWidget t m => Text -> Dynamic t Profile -> m ()
profileImg cls dProfile =
  elDynAttr "img" (("class" =: cls <>) . ("src" =:) . (^. field @"image") <$> dProfile) blank

infixl 8 ^!.
(^!.) :: Functor f => f s -> Getting b s b -> f b
x ^!. f = fmap (^. f) x
