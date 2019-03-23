{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Lens hiding (element, uncons)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Aeson as A
import Data.Bool (bool)
import Data.FileEmbed (embedFile)
import Data.Generics.Sum (_Ctor)
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Window as DOM hiding (focus)
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core
import Text.Read (readMaybe)

infixr 4 <!>
infixr 7 =?, =!, =!?

(<!>) :: (Semigroup a, Functor f) => a -> f a -> f a
a <!> b = (a <>) <$> b

(=?) :: Ord k => k -> Maybe v -> Map k v
k =? mVal = maybe mempty (k =:) mVal

(=!) :: Reflex t => k -> Dynamic t v -> Dynamic t (Map k v)
k =! dVal = (k =:) <$> dVal

(=!?) :: (Ord k, Reflex t) => k -> Dynamic t (Maybe v) -> Dynamic t (Map k v)
k =!? dmVal = maybe mempty (k =:) <$> dmVal

newtype UserId = UserId
  { unUserId :: Text
  } deriving (Eq, Ord, Show, FromJSONKey, FromJSON)

newtype ItemId = ItemId
  { unItemId :: Int
  } deriving (Eq, Ord, Show, FromJSONKey, FromJSON)

data AppState t = AppState
  { now :: Dynamic t UTCTime
  , itemMap :: Dynamic t (Map ItemId Item)
  , userMap :: Dynamic t (Map UserId User)
  , itemLists :: Dynamic t (Map FilterType [ItemId])
  , pendingReqs :: Dynamic t (Set AppRequest)
  }

data AppRequest
  = ReqItem ItemId
  | ReqItemList FilterType
  | ReqUser UserId
  deriving (Eq, Ord, Show)

data FilterType
  = FilterBest
  | FilterNew
  | FilterShow
  | FilterAsk
  | FilterJobs
  deriving (Eq, Ord, Show)

data ItemType
  = ItemStory
  | ItemComment
  | ItemJob
  | ItemPoll
  | ItemPollOpt
  deriving (Eq, Ord, Show)

data Item = Item
  { itemId :: ItemId
  , itemType :: ItemType
  , itemBy :: UserId
  , itemTime :: UTCTime
  , itemText :: Text
  , itemParent :: Maybe ItemId
  , itemPoll :: Maybe ItemId
  , itemKids :: [ItemId]
  , itemUrl :: Maybe Text
  , itemScore :: Int
  , itemTitle :: Text
  , itemParts :: [ItemId]
  , itemDescendants :: Int
  } deriving Show

instance FromJSON ItemType where
  parseJSON (A.String "story") = pure ItemStory
  parseJSON (A.String "comment") = pure ItemComment
  parseJSON (A.String "job") = pure ItemJob
  parseJSON (A.String "poll") = pure ItemPoll
  parseJSON (A.String "pollopt") = pure ItemPollOpt
  parseJSON _ = mempty

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> Item
    <$> o .: "id"
    <*> o .: "type"
    <*> o .: "by"
    <*> (posixSecondsToUTCTime <$> o .: "time")
    <*> (fromMaybe "" <$> o .:? "text")
    <*> o .:? "parent"
    <*> o .:? "poll"
    <*> (fromMaybe [] <$> o .:? "kids")
    <*> o .:? "url"
    <*> (fromMaybe 0 <$> o .:? "score")
    <*> (fromMaybe "" <$> o .:? "title")
    <*> (fromMaybe [] <$> o .:? "parts")
    <*> (fromMaybe 0 <$> o .:? "descendants")

data User = User
  { userId :: UserId
  , userCreated :: UTCTime
  , userAbout :: Maybe Text
  , userKarma :: Int
  } deriving Show

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> (posixSecondsToUTCTime <$> o .: "created")
    <*> o .:? "about"
    <*> o .: "karma"

data Route
  = RouteItemList FilterType Int
  | RouteItem ItemId
  | RouteUser UserId
  deriving (Show, Generic)

instance Semigroup Route where
  (<>) = const id

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList x = Just x

main :: IO ()
main = run 3000 $ mainWidgetWithCss $(embedFile "src/index.css") $ do
  t0 <- liftIO getCurrentTime
  dNow <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0
  pb <- getPostBuild

  rec
    let eRouteChg = leftmost [current dRoute <@ pb, updated dRoute]

    let eIMRequest = makeIMRequest <$> current dItemMap <@>
          (fmapMaybe (fmap (:[]) . preview (_Ctor @"RouteItem")) eRouteChg <>
           fmapMaybe maybeList (makePageRequest <$> current dItemLists <@> fmapMaybe (preview (_Ctor @"RouteItemList")) eRouteChg) <>
           fmapMaybe maybeList (flip makeCommentsRequest <$> current dItemMap <@> eRouteChg) <>
           fmapMaybe maybeList (makeListRequest <$> current dRoute <@> eILResponse) <>
           fmapMaybe maybeList (makeCommentsRequest <$> current dRoute <@> eIMResponse))
    let eUMRequest = makeUMRequest <$> current dUserMap <@>
          fmapMaybe (fmap (:[]) . preview (_Ctor @"RouteUser")) eRouteChg
    let eILRequest = makeILRequest <$> current dItemLists <@>
          fmapMaybe (fmap (:[]) . preview (_Ctor @"RouteItemList" . _1)) eRouteChg

    eIMResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync (traceEvent "ids" eIMRequest)
    eUMResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync eUMRequest
    eILResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync eILRequest

    dItemMap <- foldDyn (<>) M.empty eIMResponse
    dUserMap <- foldDyn (<>) M.empty eUMResponse
    dItemLists <- foldDyn (<>) M.empty eILResponse

    dPending <- foldDyn ($) S.empty
      ((S.union . S.fromList . fmap ReqItem . M.keys <$> eIMRequest) <>
       (S.difference . S.fromList . fmap ReqItem . M.keys <$> eIMResponse) <>
       (S.union . S.fromList . fmap ReqUser . M.keys <$> eUMRequest) <>
       (S.difference . S.fromList . fmap ReqUser . M.keys <$> eUMResponse) <>
       (S.union . S.fromList . fmap ReqItemList . M.keys <$> eILRequest) <>
       (S.difference . S.fromList . fmap ReqItemList . M.keys <$> eILResponse))

    let appState = AppState dNow dItemMap dUserMap dItemLists dPending

    dRoute <- makeHistoryRouter eSetRoute
    ((), eSetRoute) <- flip runReaderT appState . runEventWriterT $
      topLevel dRoute . void . dyn . ffor dRoute $ \case
        RouteUser uid -> userView uid
        RouteItemList f p -> itemList f p
        RouteItem i -> itemView i
  blank

makePageRequest :: Map FilterType [ItemId] -> (FilterType, Int) -> [ItemId]
makePageRequest ids (f, num) = maybe [] (take 30 . drop ((num - 1) * 30)) (M.lookup f ids)

makeListRequest :: Route -> Map FilterType [ItemId] -> [ItemId]
makeListRequest (RouteItemList f num) = maybe [] (take 30 . drop ((num - 1) * 30)) . M.lookup f
makeListRequest _ = const []

makeCommentsRequest :: Route -> Map ItemId Item -> [ItemId]
makeCommentsRequest (RouteItem i) = maybe [] itemKids . M.lookup i
makeCommentsRequest _ = const []

makeUMRequest :: Map UserId User -> [UserId] -> Map UserId (XhrRequest ())
makeUMRequest m = mconcat . fmap (\k -> maybe
  (k =: xhrRequest "GET" ("https://hacker-news.firebaseio.com/v0/user/" <> unUserId k <> ".json") def)
  (const M.empty) (M.lookup k m))

makeIMRequest :: Map ItemId Item -> [ItemId] -> Map ItemId (XhrRequest ())
makeIMRequest m = mconcat . fmap (\k -> maybe
  (k =: xhrRequest "GET" ("https://hacker-news.firebaseio.com/v0/item/" <> tshow (unItemId k) <> ".json") def)
  (const M.empty) (M.lookup k m))

makeILRequest :: Map FilterType [ItemId] -> [FilterType] -> Map FilterType (XhrRequest ())
makeILRequest m = mconcat . fmap (\k ->
  let url = "https://hacker-news.firebaseio.com/v0/" <> filterTypeToUrl k <> "stories.json"
  in maybe (k =: xhrRequest "GET" url def) (const M.empty) (M.lookup k m))

topLevel :: (EventWriter t Route m, MonadWidget t m) => Dynamic t Route -> m () -> m ()
topLevel dRoute contents =
  elAttr "div" ("id" =: "app") $ do
    elClass "header" "header" $
      elClass "nav" "inner" $ do
        let sel = demux (routeToFilter <$> dRoute)
        appLink (RouteItemList FilterBest 1) (pure mempty) (pure True)
          (elAttr "img" ("class" =: "logo" <> "alt" =: "Logo" <> "src" =: "/logo-48.png") blank)
        appLink (RouteItemList FilterBest 1) (demuxActive sel FilterBest) (pure True) (text "Top")
        appLink (RouteItemList FilterNew 1) (demuxActive sel FilterNew) (pure True) (text "New")
        appLink (RouteItemList FilterShow 1) (demuxActive sel FilterShow) (pure True) (text "Show")
        appLink (RouteItemList FilterAsk 1) (demuxActive sel FilterAsk) (pure True) (text "Ask")
        appLink (RouteItemList FilterJobs 1) (demuxActive sel FilterJobs) (pure True) (text "Jobs")
        elAttr "a" ("class" =: "github" <> "href" =: "https://github.com/zarybnicky/thesis" <>
                    "target" =: "_blank" <> "rel" =: "noopener") (text "Built with Reflex")
    divClass "view" contents
  where
    demuxActive sel f = bool mempty ("class" =: "router-link-active") <$> demuxed sel (Just f)
    routeToFilter (RouteItemList f _) = Just f
    routeToFilter _ = Nothing

itemList :: (MonadReader (AppState t) m, EventWriter t Route m, MonadWidget t m) => FilterType -> Int -> m ()
itemList filterType pageNum =
  divClass "news-view" $ do
    dItemMap <- asks itemMap
    dItemLists <- asks itemLists
    let dAllIds = fromMaybe [] . M.lookup filterType <$> dItemLists
        itemsPerPage = 30 :: Double
        dMaxPage = ceiling . (/ itemsPerPage) . fromIntegral . length <$> dAllIds
        dCurrentIds = take 30 . drop ((pageNum - 1) * 30) <$> dAllIds
        dItems = restrictItems <$> dItemMap <*> dCurrentIds
    divClass "news-list-nav" $ do
      let dCanPrev = pure (1 < pageNum)
          dCanNext = (pageNum <) <$> dMaxPage
      appLink (RouteItemList filterType (pageNum - 1)) (disAttr dCanPrev) dCanPrev (text "< prev")
      el "span" $ display (pure pageNum) >> text "/" >> display dMaxPage
      appLink (RouteItemList filterType (pageNum + 1)) (disAttr dCanNext) dCanNext (text "more >")
    void $ divClass "news-list" $
      el "ul" (simpleList dItems itemListItem)
  where
    disAttr :: Reflex t => Dynamic t Bool -> Dynamic t (Map Text Text)
    disAttr = fmap (bool ("class" =: "disabled") mempty)

itemListItem :: (EventWriter t Route m, MonadReader (AppState t) m, MonadWidget t m) => Dynamic t Item -> m ()
itemListItem dItem =
  elClass "li" "news-item" $ do
    elClass "span" "score" $ display (itemScore <$> dItem)
    text " "
    elClass "span" "title" $ do
      _ <- dyn $ ffor dItem $ \item -> case itemUrl item of
        Just url -> elAttr "a"
          ("target" =: "_blank" <> "rel" =: "noopener" <> "href" =: url) (text $ itemTitle item)
        Nothing -> appLink (RouteItem $ itemId item) (pure mempty) (pure True) (text $ itemTitle item)
      hostView dItem
    el "br" blank
    elClass "span" "meta" $ do
      elDynAttr "span" ("class" =: "by" <!> (bool mempty ("style" =: "display:none") .
                        (== ItemJob) . itemType <$> dItem)) $ do
        text "by "
        userLink (itemBy <$> dItem)
      text " "
      elClass "span" "time" (timeAgoView (itemTime <$> dItem))
      text " "
      elDynAttr "span" ("class" =: "comments-link" <!> (bool mempty ("style" =: "display:none") .
                        (== ItemJob) . itemType <$> dItem)) $ do
        text "| "
        appLinkDyn (RouteItem . itemId <$> dItem) (pure mempty) (pure True) $ do
          display $ itemDescendants <$> dItem
          text " comments"

itemView :: (EventWriter t Route m, MonadReader (AppState t) m, MonadWidget t m) => ItemId -> m ()
itemView iid =
  divClass "item-view" $ do
    dItemMap <- asks itemMap
    dPendingReqs <- asks pendingReqs
    let dmItem = M.lookup iid <$> dItemMap
    void $ dyn $ ffor dmItem . maybe blank $ \(item :: Item) -> do
      divClass "item-view-header" $ do
        elAttr "a"
          ("target" =: "_blank" <> "href" =? itemUrl item)
          (el "h1" (text $ itemTitle item))
        hostView (pure item)
        elClass "p" "meta" $ do
          text (tshow $ itemScore item)
          text " points | by "
          userLink (pure $ itemBy item)
          text " "
          timeAgoView (pure $ itemTime item)
      divClass "item-view-comments" $ do
        elClass "p" "item-view-comments-header" $ do
          text (commentsHeader item)
          spinner $ ffor dPendingReqs (ReqItem iid `elem`)
        let kids = flip restrictItems (itemKids item) <$> dItemMap
        void $ elClass "ul" "comment-children" (simpleList kids commentView)
  where
    commentsHeader item =
      if itemDescendants item > 0
        then tshow (itemDescendants item) <> " comments"
        else "No comments yet"

commentView :: (EventWriter t Route m, MonadReader (AppState t) m, MonadWidget t m) => Dynamic t Item -> m ()
commentView dItem =
  elClass "li" "comment" $ do
    dItemMap <- asks itemMap
    divClass "by" $ do
      userLink (itemBy <$> dItem)
      text " "
      timeAgoView (itemTime <$> dItem)
    _ <- elDynHtmlAttr' "div" ("class" =: "text") (itemText <$> dItem)
    rec
      dOpen <- foldDyn (const not) True eToggle
      eToggle <- elDynAttr "div" (toggleAttrs <$> dItem <*> dOpen) $ do
        (e, ()) <- el' "a" (dynText $ toggleText <$> dItem <*> dOpen)
        pure (domEvent Click e)
    _ <- dyn . ffor dOpen . bool blank . void $ do
      let kids = restrictItems <$> dItemMap <*> (itemKids <$> dItem)
      elClass "ul" "comment-children" (simpleList kids commentView)
    pure ()
  where
    toggleAttrs item open = mconcat
      [ "class" =: ("toggle" <> bool "" " open" open)
      , "style" =: bool "" "display:none" (null $ itemKids item)
      ]
    toggleText _ True = "[-]"
    toggleText item False =
      if length (itemKids item) == 1
        then "[+] 1 reply collapsed"
        else "[+] " <> tshow (length $ itemKids item) <> " replies collapsed"

userView :: (MonadReader (AppState t) m, MonadWidget t m) => UserId -> m ()
userView uid = do
  dUserMap <- asks userMap
  void . dyn . ffor (M.lookup uid <$> dUserMap) $ \case
    Nothing -> el "h1" (text "User not found.")
    Just user -> do
      el "h1" (text $ "User : " <> unUserId uid)
      elClass "ul" "meta" $ do
        el "li" $ do
          elClass "span" "label" (text "Created: ")
          timeAgoView (pure $ userCreated user)
        el "li" $ do
          elClass "span" "label" (text "Karma: ")
          text (tshow (userKarma user))
        case userAbout user of
          Nothing -> blank
          Just about -> void $ elDynHtmlAttr' "div" ("class" =: "about") (pure about)
      elClass "p" "links" $ do
        elAttr "a" ("href" =: ("https://news.ycombinator.com/submitted?id=" <> unUserId uid)) (text "submissions")
        text " | "
        elAttr "a" ("href" =: ("https://news.ycombinator.com/threads?id=" <> unUserId uid)) (text "comments")

makeHistoryRouter ::
     ( MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     )
  => Event t Route
  -> m (Dynamic t Route)
makeHistoryRouter eSetRoute = do
  window <- liftJSM DOM.currentWindowUnchecked
  location <- liftJSM (DOM.getLocation window)
  history <- liftJSM (DOM.getHistory window)
  iRoute <- liftJSM (getRoute location)
  eRoute <- wrapDomEvent window (`DOM.on` DOM.popState) (getRoute location)
  performEvent_ $ DOM.pushState history () ("" :: Text) . Just . encodeRoute <$> eSetRoute
  holdDyn iRoute $ leftmost [eSetRoute, eRoute]
  where
    getRoute loc = (decodeRoute .) . (<>) <$> DOM.getPathname loc <*> DOM.getSearch loc

appLink ::
     forall t m. (EventWriter t Route m, MonadWidget t m)
  => Route
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLink = appLinkDyn . pure

appLinkDyn ::
     forall t m. (EventWriter t Route m, MonadWidget t m)
  => Dynamic t Route
  -> Dynamic t (Map Text Text)
  -> Dynamic t Bool
  -> m ()
  -> m ()
appLinkDyn dR dAttrs dDisabled inner = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $
    ffor2 dR dAttrs (\r attrs -> "href" =: encodeRoute r <> attrs)
  (e, ()) <- element "a" ((def :: ElementConfig EventResult t (DomBuilderSpace m))
    & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
    & elementConfig_eventSpec %~
        addEventSpecFlags
        (Proxy :: Proxy (DomBuilderSpace m))
        Click
        (const preventDefault)) inner
  tellEvent $ current dR <@ gate (current dDisabled) (domEvent Click e)

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

hostView :: MonadWidget t m => Dynamic t Item -> m ()
hostView dItem =
  elDynAttr "span" ("class" =: "host" <!> (maybe ("style" =: "display:none") (const mempty) .
                    itemUrl <$> dItem)) $ do
    text " ("
    dynText (maybe "" getUrlHost . itemUrl <$> dItem)
    text ")"

userLink :: (EventWriter t Route m, MonadWidget t m) => Dynamic t UserId -> m ()
userLink dUserId =
  appLinkDyn (RouteUser <$> dUserId) (pure mempty) (pure True) (dynText $ unUserId <$> dUserId)

timeAgoView :: (MonadReader (AppState t) m, MonadWidget t m) => Dynamic t UTCTime -> m ()
timeAgoView dItemTime = do
  now <- sample . current =<< asks now
  dynText (getTimeAgo now <$> dItemTime)
  text " ago"
  where
    getTimeAgo :: UTCTime -> UTCTime -> Text
    getTimeAgo tnow before =
      let diff = diffUTCTime tnow before
      in if | diff < 3600 -> pluralize (floor $ diff / 60) " minute"
            | diff < 86400 -> pluralize (floor $ diff / 3600) " hour"
            | True -> pluralize (floor $ diff / 86400) " day"

restrictItems :: Map ItemId Item -> [ItemId] -> [Item]
restrictItems items = M.elems . M.restrictKeys items . S.fromList

filterTypeToUrl :: FilterType -> Text
filterTypeToUrl = \case
  FilterBest -> "best"
  FilterNew -> "new"
  FilterShow -> "show"
  FilterAsk -> "ask"
  FilterJobs -> "jobs"

decodeRoute :: Text -> Route
decodeRoute x = case uncons (drop 1 $ T.splitOn "/" x) of
  Just ("best", rest) -> RouteItemList FilterBest (parsePageNum rest)
  Just ("new", rest) -> RouteItemList FilterNew (parsePageNum rest)
  Just ("show", rest) -> RouteItemList FilterShow (parsePageNum rest)
  Just ("ask", rest) -> RouteItemList FilterAsk (parsePageNum rest)
  Just ("jobs", rest) -> RouteItemList FilterJobs (parsePageNum rest)
  Just ("item", rest) -> RouteItem (ItemId $ parsePageNum rest)
  Just ("user", rest) -> RouteUser (UserId . T.intercalate "" $ take 1 rest)
  _ -> RouteItemList FilterBest 1
  where
    parsePageNum :: [Text] -> Int
    parsePageNum = max 1 . fromMaybe 1 . readMaybe . T.unpack . T.intercalate "" . take 1

encodeRoute :: Route -> Text
encodeRoute (RouteItemList f p) = "/" <> filterTypeToUrl f <> "/" <> tshow p
encodeRoute (RouteItem u) = "/item/" <> tshow (unItemId u)
encodeRoute (RouteUser u) = "/user/" <> unUserId u

getUrlHost :: Text -> Text
getUrlHost = T.intercalate "/"
  . take 1
  . filter (\x -> not (T.null x) && x /= "www" && x /= "https:" && x /= "http:")
  . T.split (== '/')

pluralize :: Int -> Text -> Text
pluralize 1 x = "1" <> x <> "s"
pluralize n x = tshow n <> x <> "s"

tshow :: Show a => a -> Text
tshow = T.pack . show

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"
