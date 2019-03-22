{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), fromGregorian, getCurrentTime, diffUTCTime)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

newtype UserId = UserId
  { unUserId :: Text
  } deriving (Eq)

newtype ItemId = ItemId
  { unItemId :: Int
  } deriving (Eq, Ord)

data AppState t = AppState
  { dNow :: Dynamic t UTCTime
  , dItemMap :: Dynamic t (Map ItemId Item)
  , dItemLists :: Dynamic t (Map FilterType [ItemId])
  , dPending :: Dynamic t [AppRequest]
  }

data AppRequest
  = ReqItem ItemId
  | ReqComments ItemId
  | ReqUser UserId
  deriving Eq

data FilterType
  = FilterBest
  | FilterNew
  | FilterShow
  | FilterAsk
  | FilterJobs
  deriving (Eq, Ord)

filterTypeToUrl :: FilterType -> Text
filterTypeToUrl = \case
  FilterBest -> "best"
  FilterNew -> "new"
  FilterShow -> "show"
  FilterAsk -> "ask"
  FilterJobs -> "jobs"

data ItemType
  = ItemStory
  | ItemComment
  | ItemJob
  | ItemPoll
  | ItemPollOpt
  deriving Eq

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
  }

data User = User
  { userId :: UserId
  , userCreated :: UTCTime
  , userAbout :: Maybe Text
  , userKarma :: Int
  }

main :: IO ()
main = run 3000 $ mainWidgetWithCss css $ do
  t0 <- liftIO getCurrentTime
  dNow' <- fmap _tickInfo_lastUTC <$> clockLossy 1 t0

  let item = Item
        { itemId = ItemId 5
        , itemType = ItemStory
        , itemBy = UserId "zarybnicky"
        , itemTime = UTCTime (fromGregorian 2019 03 21) 0
        , itemText = "<b>Comment</b>"
        , itemParent = Nothing
        , itemPoll = Nothing
        , itemKids = []
        , itemUrl = Nothing
        , itemScore = 5
        , itemTitle = "Title"
        , itemParts = []
        , itemDescendants = 5
        }
  let appState = AppState dNow' (pure M.empty) (pure M.empty) (pure [])
  itemListItem appState (pure item)

css :: ByteString
css = ""

itemList :: MonadWidget t m => AppState t -> FilterType -> Int -> m ()
itemList s filterType pageNum =
  divClass "news-view" $ do
    let dAllIds = (fromMaybe [] <$>) (M.lookup filterType <$> dItemLists s)
        itemsPerPage = 30 :: Double
        dMaxPage = ceiling . (/ itemsPerPage) . fromIntegral . length <$> dAllIds
        dCurrentIds = take 30 . drop ((pageNum - 1) * 30) <$> dAllIds
        dItems = restrictItems <$> dItemMap s <*> dCurrentIds
    divClass "news-list-nav" $ do
      elDynAttr "a" (pure makePrevAttrs) (text "< prev")
      el "span" $ display (pure pageNum) >> text "/" >> display dMaxPage
      elDynAttr "a" (makeNextAttrs <$> dMaxPage) (text "more >")
    void $ divClass "news-list" (simpleList dItems (itemListItem s))
  where
    makePrevAttrs =
      if 1 < pageNum
        then "href" =: ("/" <> filterTypeToUrl filterType <> "/" <> tshow (pageNum - 1))
        else "class" =: "disabled"
    makeNextAttrs maxPage =
      if pageNum < maxPage
        then "href" =: ("/" <> filterTypeToUrl filterType <> "/" <> tshow (pageNum + 1))
        else "class" =: "disabled"

itemListItem :: MonadWidget t m => AppState t -> Dynamic t Item -> m ()
itemListItem s dItem =
  elClass "li" "news-item" $ do
    elClass "span" "score" $ display (itemScore <$> dItem)
    text " "
    elClass "span" "title" $ do
      elDynAttr "a" (getLinkAttrs <$> dItem) (dynText $ itemTitle <$> dItem)
      hostView dItem
    el "br" blank
    elClass "span" "meta" $ do
      elDynAttr "span" (("class" =: "by" <>) . bool mempty ("style" =: "display:none") .
                        (== ItemJob) . itemType <$> dItem) $ do
        text "by "
        userLink (itemBy <$> dItem)
      text " "
      elClass "span" "time" (timeAgoView (dNow s) (itemTime <$> dItem))
      text " "
      elDynAttr "span" (("class" =: "comments-link" <>) . bool mempty ("style" =: "display:none") .
                        (== ItemJob) . itemType <$> dItem) $ do
        text "| "
        elDynAttr "a" (("href" =:) . ("/item/" <>) . tshow . unItemId . itemId <$> dItem) $ do
          display $ itemDescendants <$> dItem
          text " comments"
  where
    getLinkAttrs :: Item -> Map Text Text
    getLinkAttrs x = case itemUrl x of
      Nothing -> "href" =: ("/item/" <> (tshow . unItemId $ itemId x))
      Just url -> "target" =: "_blank" <> "rel" =: "noopener" <> "href" =: url

itemView :: MonadWidget t m => AppState t -> Dynamic t Item -> m ()
itemView s dItem =
  divClass "item-view" $ do
    divClass "item-view-header" $ do
      elDynAttr "a"
        (("target" =: "_blank" <>) . maybe mempty ("href" =:) . itemUrl <$> dItem)
        (el "h1" (dynText $ itemTitle <$> dItem))
      hostView dItem
      elClass "p" "meta" $ do
        display $ itemScore <$> dItem
        text " points | by"
        userLink (itemBy <$> dItem)
        text " "
        timeAgoView (dNow s) (itemTime <$> dItem)
    divClass "item-view-comments" $ do
      elClass "p" "item-view-comments-header" $ do
        dynText (commentsHeader <$> dItem)
        spinner $ ffor2 dItem (dPending s) $ \i reqs ->
          ReqComments (itemId i) `elem` reqs
      let kids = restrictItems <$> dItemMap s <*> (itemKids <$> dItem)
      void $ elClass "ul" "comment-children" (simpleList kids (commentView s))
  where
    commentsHeader item =
      if itemDescendants item > 0
        then tshow (itemDescendants item) <> " comments"
        else "No comments yet"

commentView :: MonadWidget t m => AppState t -> Dynamic t Item -> m ()
commentView s dItem =
  elClass "li" "comment" $ do
    divClass "by" $ do
      userLink (itemBy <$> dItem)
      text " "
      timeAgoView (dNow s) (itemTime <$> dItem)
    _ <- elDynHtmlAttr' "div" ("class" =: "text") (itemText <$> dItem)
    rec
      dOpen <- foldDyn (const not) True eToggle
      eToggle <- elDynAttr "div" (toggleAttrs <$> dItem <*> dOpen) $ do
        (e, ()) <- el' "button" (dynText $ toggleText <$> dItem <*> dOpen)
        pure (domEvent Click e)
    _ <- dyn . ffor dOpen . bool blank . void $ do
      let kids = restrictItems <$> dItemMap s <*> (itemKids <$> dItem)
      elClass "ul" "comment-children" (simpleList kids (commentView s))
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

userView :: MonadWidget t m => Dynamic t UTCTime -> Dynamic t (Maybe User) -> m ()
userView dNow' dmUser = void . dyn . ffor dmUser $ \case
  Nothing -> el "h1" (text "User not found.")
  Just user -> do
    let uid = unUserId (userId user)
    el "h1" (text $ "User : " <> uid)
    elClass "ul" "meta" $ do
      el "li" $ do
        elClass "span" "label" (text "Created: ")
        timeAgoView dNow' (pure $ userCreated user)
      el "li" $ do
        elClass "span" "label" (text "Karma: ")
        text (tshow $ userKarma user)
      case userAbout user of
        Nothing -> blank
        Just about -> void $ elDynHtmlAttr' "div" ("class" =: "about") (pure about)
    elClass "p" "links" $ do
      elAttr "a" ("href" =: ("https://news.ycombinator.com/submitted?id=" <> uid)) (text "submissions")
      text " | "
      elAttr "a" ("href" =: ("https://news.ycombinator.com/threads?id=" <> uid)) (text "comments")

spinner :: MonadWidget t m => Dynamic t Bool -> m ()
spinner dShow =
  elDynAttrNS svgNS "svg" ((baseAttrs <>) . ("class" =:) . bool "spinner" "spinner show" <$> dShow) $
    elDynAttrNS svgNS "circle" (pure circleAttrs) blank
  where
    baseAttrs, circleAttrs :: Map Text Text
    baseAttrs = "width" =: "44px" <> "height" =: "44px" <> "viewBox" =: "0 0 44 44"
    circleAttrs = "class" =: "path" <> "fill" =: "none" <> "cx" =: "22" <>
                   "cy" =: "22" <> "r" =: "20" <> "stroke-width" =: "4" <>
                   "stroke-linecap" =: "round"

hostView :: MonadWidget t m => Dynamic t Item -> m ()
hostView dItem =
  elDynAttr "span" (("class" =: "host" <>) . maybe ("style" =: "display:none") (const mempty) .
                    itemUrl <$> dItem) $ do
    text " ("
    dynText $ maybe "" getUrlHost . itemUrl <$> dItem
    text ")"

userLink :: MonadWidget t m => Dynamic t UserId -> m ()
userLink dUserId =
  let userName = unUserId <$> dUserId
  in elDynAttr "a" (("href" =:) . ("/user/" <>) <$> userName) (dynText userName)

timeAgoView :: MonadWidget t m => Dynamic t UTCTime -> Dynamic t UTCTime -> m ()
timeAgoView dNow' dItemTime = do
  dynText (getTimeAgo <$> dNow' <*> dItemTime)
  text " ago"
  where
    getTimeAgo :: UTCTime -> UTCTime -> Text
    getTimeAgo now before =
      let diff = diffUTCTime now before
      in if | diff < 3600 -> pluralize (floor $ diff / 60) " minute"
            | diff < 86400 -> pluralize (floor $ diff / 3600) " hour"
            | True -> pluralize (floor $ diff / 86400) " day"

restrictItems :: Map ItemId Item -> [ItemId] -> [Item]
restrictItems itemMap itemIds = M.elems $ M.restrictKeys itemMap (S.fromList itemIds)

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
