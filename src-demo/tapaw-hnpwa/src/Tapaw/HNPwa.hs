{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapaw.HNPwa
  ( frontend
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import Tapaw.HNPwa.Navigation (makeHistoryRouter, appLink, appLinkDyn)
import Tapaw.HNPwa.Types
import Tapaw.HNPwa.Utils ((<!>), (=?), (=!), getUrlHost, pluralize, tshow)

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" (text "HNPWA")
  elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width,initial-scale=1") blank
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/index.css") blank

frontend :: JSM ()
frontend = mainWidgetWithHead headWidget $ do
  t0 <- liftIO getCurrentTime
  dNow <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0

  rec
    let eIMRequest = fmapMaybe dedupeRequests (updated $ makeIMRequest <$> dItemMap <*> dItemLists <*> dRoute)
    let eUMRequest = fmapMaybe dedupeRequests (updated $ makeUMRequest <$> dUserMap <*> dRoute)
    let eILRequest = fmapMaybe dedupeRequests (updated $ makeILRequest <$> dItemLists <*> dRoute)

    eIMResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync eIMRequest
    eUMResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync eUMRequest
    eILResponse <- fmap (M.mapMaybe decodeXhrResponse) <$> performRequestsAsync eILRequest

    dItemMap   <- foldDyn (<>) M.empty eIMResponse
    dUserMap   <- foldDyn (<>) M.empty eUMResponse
    dItemLists <- foldDyn (<>) M.empty eILResponse

    dPending <- foldDyn ($) S.empty $ mergeWith (.)
      [ flip S.difference . S.fromList . fmap ReqItem . M.keys <$> eIMResponse
      , flip S.difference . S.fromList . fmap ReqUser . M.keys <$> eUMResponse
      , flip S.difference . S.fromList . fmap ReqItemList . M.keys <$> eILResponse
      , S.union . S.fromList . fmap ReqItem . M.keys <$> eIMRequest
      , S.union . S.fromList . fmap ReqUser . M.keys <$> eUMRequest
      , S.union . S.fromList . fmap ReqItemList . M.keys <$> eILRequest
      ]
    let appState = AppState dNow dItemMap dUserMap dItemLists dPending

    dRoute <- makeHistoryRouter (RouteItemList FilterBest 1) eSetRoute
    ((), eSetRoute) <- flip runReaderT appState . runEventWriterT $
      topLevel dRoute . void . dyn . ffor dRoute $ \case
        RouteUser uid -> userView uid
        RouteItemList f p -> itemList f p
        RouteItem i -> itemView i
  blank
  where
    dedupeRequests x = if M.null x then Nothing else Just x

makeIMRequest :: Map ItemId Item -> Map FilterType [ItemId] -> Route -> Map ItemId (XhrRequest ())
makeIMRequest items lists r = mconcat . fmap makeRequest $ case r of
  RouteItem iId -> iId : lookupKids iId
  RouteItemList f n -> case M.lookup f lists of
    Nothing -> []
    Just ids -> take 30 $ drop ((n - 1) * 30) ids
  _ -> []
  where
  lookupKids k = case M.lookup k items of
    Nothing -> []
    Just i -> itemKids i ++ (lookupKids =<< itemKids i)
  makeRequest k = case M.lookup k items of
    Nothing -> k =: xhrRequest "GET" ("https://hacker-news.firebaseio.com/v0/item/" <> tshow (unItemId k) <> ".json") def
    Just _ -> M.empty

makeUMRequest :: Map UserId User -> Route -> Map UserId (XhrRequest ())
makeUMRequest m (RouteUser k) = case M.lookup k m of
  Nothing -> k =: xhrRequest "GET" ("https://hacker-news.firebaseio.com/v0/user/" <> unUserId k <> ".json") def
  Just _ -> M.empty
makeUMRequest _ _ = M.empty

makeILRequest :: Map FilterType [ItemId] -> Route -> Map FilterType (XhrRequest ())
makeILRequest m (RouteItemList k _) = case M.lookup k m of
  Nothing -> k =: xhrRequest "GET" ("https://hacker-news.firebaseio.com/v0/" <> filterTypeToUrl k <> "stories.json") def
  Just _ -> M.empty
makeILRequest _ _ = M.empty

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

itemList ::
     (MonadReader (AppState t) m, EventWriter t Route m, MonadWidget t m)
  => FilterType
  -> Int
  -> m ()
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

itemListItem ::
     (EventWriter t Route m, MonadReader (AppState t) m, MonadWidget t m)
  => Dynamic t Item
  -> m ()
itemListItem dItem =
  elClass "li" "news-item" $ do
    elClass "span" "score" $ display (itemScore <$> dItem)
    text " "
    elClass "span" "title" $ do
      dItem' <- holdUniqDyn dItem
      _ <- dyn $ ffor dItem' $ \item -> case itemUrl item of
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

itemView ::
     (EventWriter t Route m, MonadReader (AppState t) m, MonadWidget t m)
  => ItemId
  -> m ()
itemView iid =
  divClass "item-view" $ do
    dItemMap <- asks itemMap
    dPendingReqs <- asks pendingReqs
    dmItem <- holdUniqDyn $ M.lookup iid <$> dItemMap
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
          spinner (not . null <$> dPendingReqs)
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

hostView :: (DomBuilder t m, PostBuild t m) => Dynamic t Item -> m ()
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
  now' <- sample . current =<< asks now
  dynText (getTimeAgo now' <$> dItemTime)
  text " ago"
  where
    getTimeAgo :: UTCTime -> UTCTime -> Text
    getTimeAgo tnow before =
      let diff = diffUTCTime tnow before
      in case () of
        () | diff < 3600 -> pluralize (floor $ diff / 60) " minute"
           | diff < 86400 -> pluralize (floor $ diff / 3600) " hour"
           | otherwise -> pluralize (floor $ diff / 86400) " day"

restrictItems :: Map ItemId Item -> [ItemId] -> [Item]
restrictItems items = M.elems . M.restrictKeys items . S.fromList

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"
