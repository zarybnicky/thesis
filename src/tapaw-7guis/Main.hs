{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens hiding (element)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Generics.Product as X (field)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, getCurrentTime, defaultTimeLocale, formatTime, fromGregorian, parseTimeM)
import GHC.Generics (Generic)
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.MouseEvent as DOM
import GHCJS.DOM.Types (uncheckedCastTo)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core
import Text.Read (readMaybe)

main :: IO ()
main = run 3000 $ mainWidgetWithCss css $
  divClass "content" .
  tabDisplay "menu" "selected" . mconcat . zipWith (=:) [(1 :: Int)..] $
    [ ("1) Counter", counter)
    , ("2) Temperature Converter", tempConverter)
    , ("3) Flight Booker", flightBooker)
    , ("4) Timer", timer)
    , ("5) CRUD", crud)
    , ("6) Circle Drawer", circleDrawer)
    , ("7) Cells", cells)
    ]

css :: ByteString
css = mconcat
  [ ".content { display: flex; }"
  , ".content > ul { flex: 0 0 250px; }"
  , ".content > div { flex: 1 1 auto; padding: 10px; }"
  , ".menu { list-style: none; padding: 0; border: 1px solid #eee; }"
  , ".menu li { margin: 0; border: 1px solid #eee; }"
  , ".menu li.selected { font-weight: bold }"
  , ".menu a { padding: .4rem; display: block; }"
  ]


counter :: MonadWidget t m => m ()
counter = do
  eClick <- button "Click Me"
  text "Clicks: "
  display =<< foldDyn (+) (0 :: Int) (1 <$ eClick)


doubleInput :: MonadWidget t m => Event t Double -> m (Event t Double)
doubleInput eSet = do
  inp <- inputElement $ def { _inputElementConfig_setValue = Just (tshow <$> eSet) }
  pure $ fmapMaybe (readMaybe . T.unpack) (_inputElement_input inp)

tempConverter :: MonadWidget t m => m ()
tempConverter = do
  rec celsius <- doubleInput (f2c <$> fahrenheit)
      text " Celsius = "
      fahrenheit <- doubleInput (c2f <$> celsius)
  text " Fahrenheit"
  where
    f2c, c2f :: Double -> Double
    f2c x = (x - 32) * 5 / 9
    c2f x = x * 9 / 5 + 32


data FlightType = OneWay | Return
  deriving (Eq, Ord)

dateInput :: MonadWidget t m => Day -> Dynamic t Bool -> m (Dynamic t (Maybe Day))
dateInput initial disabled = do
  rec let attrs = bool mempty ("disabled" =: "disabled") <$> disabled
      let attrs' = maybe (<> "style" =: "background-color:red") (const id) <$> i' <*> attrs
      modifyAttrs <- dynamicAttributesToModifyAttributes attrs'
      i <- fmap value . inputElement $ def
        & inputElementConfig_initialValue .~ showTime initial
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
      let i' = fmap (parseTimeM True defaultTimeLocale "%d.%m.%Y" . T.unpack) i
  pure i'

flightBooker :: forall t m. MonadWidget t m => m ()
flightBooker = do
  let initial = fromGregorian 2019 3 15
  typ <- value <$> dropdown OneWay (constDyn (OneWay =: "one-way flight" <> Return =: "return flight")) def
  el "br" blank
  dateFrom <- dateInput initial (constDyn False)
  el "br" blank
  dateTo <- dateInput initial ((== OneWay) <$> typ)
  el "br" blank

  let result = f <$> typ <*> dateFrom <*> dateTo
  eSubmit <- buttonDisable result "Book flight"
  void $ widgetHold blank $ fromMaybe blank <$> leftmost [
      Nothing <$ updated result,
      Just <$> eSubmit
    ]
  where
    f OneWay (Just x) _ = Just (text $ "You have booked a one-way flight on " <> showTime x)
    f Return (Just x) (Just y)
      | x < y = Just (text $ "You have booked a return flight on " <> showTime x <> " and " <> showTime y)
      | otherwise = Nothing
    f _ _ _ = Nothing

showTime :: Day -> T.Text
showTime = T.pack . formatTime defaultTimeLocale "%d.%m.%Y"

buttonDisable :: MonadWidget t m => Dynamic t (Maybe a) -> Text -> m (Event t a)
buttonDisable dDisabled txt = do
  let attrs = maybe ("disabled" =: "disabled") (const mempty) <$> dDisabled
  (e, ()) <- elDynAttr' "button" attrs (text txt)
  pure . fmapMaybe id $ current dDisabled <@ domEvent Click e


timer :: MonadWidget t m => m ()
timer = do
  t0 <- liftIO getCurrentTime
  eTick <- tickLossy 0.1 t0

  counterMax <- value <$> rangeInput def
  eReset <- button "Reset"
  rec counter <- foldDyn (bool (const 0) (+ 0.1)) 0 $ leftmost
        [ True <$ eReset
        , False <$ gate ((<) <$> current counter <*> current counterMax) eTick
        ]
  elAttr "div" ("style" =: "width:100%;background-color:#ddd") $
    elDynAttr "dir" (ffor counter $ \x -> "style" =: ("background-color:#red;width:" <> tshow x <> "%"))
      blank
  dynText $ tshow . roundDouble <$> counter
  where
    roundDouble :: Float -> Float
    roundDouble = (/ 10) . fromIntegral . (round :: Float -> Int) . (* 10)


crud :: MonadWidget t m => m ()
crud = do
  rec db <- foldDyn ($) ((0 :: Int) =: ("Emil", "Hans")) $ leftmost
        [ M.insert <$> nextId <*> current dPerson <@ eCreate
        , M.adjust . const <$> current dPerson <@> eUpdate
        , M.delete <$> eDelete
        ]
      let nextId = (+ 1) . foldr max 0 . M.keys <$> current db
      dFilter <- value <$> inputElement def
      let dbFiltered = (\f -> M.filter (T.isPrefixOf f . snd)) <$> dFilter <*> db
      dSelected <- foldDyn (\x y -> if x == y then Nothing else x) Nothing (Just <$> eSelect)
      eSelect <- elAttr "select" ("multiple" =: "multiple") $
        selectViewListWithMaybeKey_ dSelected dbFiltered personListItem
      dPerson <- personForm $ (\m -> maybe ("", "") (m M.!)) <$> current dbFiltered <@> updated dSelected
      eCreate <- button "Create"
      eUpdate <- buttonDisable dSelected "Update"
      eDelete <- buttonDisable dSelected "Delete"
  pure ()

personListItem :: MonadWidget t m => Dynamic t (Text, Text) -> Dynamic t Bool -> m (Event t ())
personListItem dContents dSelected = do
  (e, ()) <- elDynAttr' "option"
    (bool mempty ("selected" =: "selected") <$> dSelected)
    (dynText $ (\(name, surname) -> surname <> ", " <> name) <$> dContents)
  pure $ () <$ domEvent Click e

personForm :: MonadWidget t m => Event t (Text, Text) -> m (Dynamic t (Text, Text))
personForm eSet = do
  dName <- fmap value . inputElement $ def & inputElementConfig_setValue .~ (fst <$> eSet)
  dSurname <- fmap value . inputElement $ def & inputElementConfig_setValue .~ (snd <$> eSet)
  pure $ (,) <$> dName <*> dSurname

selectViewListWithMaybeKey_ ::
     (MonadWidget t m, Ord k)
  => Dynamic t (Maybe k)
  -> Dynamic t (M.Map k v)
  -> (Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -> m (Event t k)
selectViewListWithMaybeKey_ selection vals mkChild = do
  let selectionDemux = demux selection
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux (Just k)
    fmap (k <$) (mkChild v selected)
  pure $ switchPromptlyDyn $ leftmost . M.elems <$> selectChild


data Circle = Circle
  { circleX :: Int
  , circleY :: Int
  , circleRadius :: Float
  } deriving (Show, Generic)

data CircleAction
  = CircleCreate Circle
  | CircleAdjust Int Float

data CircleState = CircleState
  { csUndo :: [M.Map Int Circle]
  , csRedo :: [M.Map Int Circle]
  , csCurrent :: M.Map Int Circle
  , csNextIndex :: Int
  } deriving (Show, Generic)

undo :: M.Map Int Circle -> CircleState -> CircleState
undo s x = x
  & field @"csCurrent" .~ s
  & field @"csUndo" %~ drop 1
  & field @"csRedo" %~ (csCurrent x:)

redo :: M.Map Int Circle -> CircleState -> CircleState
redo s x = x
  & field @"csCurrent" .~ s
  & field @"csRedo" %~ drop 1
  & field @"csUndo" %~ (csCurrent x:)

newAction :: CircleAction -> CircleState -> CircleState
newAction act s =
  let next = s & field @"csUndo" %~ (csCurrent s:) & field @"csRedo" .~ []
  in case act of
    CircleCreate c -> next
     & field @"csNextIndex" %~ (+1)
     & field @"csCurrent" %~ M.insert (csNextIndex s) c
    CircleAdjust idx r -> next
     & field @"csCurrent" %~ M.adjust (field @"circleRadius" .~ r) idx

circleDrawer :: forall t m  . MonadWidget t m => m ()
circleDrawer = do
  let initCs = CircleState [] [] M.empty 0
  rec cs <- foldDyn ($) initCs $ leftmost
        [ undo <$> eUndo
        , redo <$> eRedo
        , newAction . CircleCreate <$> eInsert
        , fmapMaybe (fmap newAction) $ (\idx r -> flip CircleAdjust r <$> idx) <$> current dSelected <@> eRadius
        ]
      eUndo <- buttonDisable ((^? field @"csUndo" . _head) <$> cs) "Undo"
      eRedo <- buttonDisable ((^? field @"csRedo" . _head) <$> cs) "Redo"

      (svg, eSelect) <- elDynAttrNS' svgNS "svg" (pure $ "style" =: "display:block;width:600px;height:300px") $ do
        elDynAttrNS svgNS "rect" (pure $ "width" =: "600" <> "height" =: "300" <> "stroke" =: "black" <> "fill" =: "none") blank
        selectViewListWithMaybeKey_ dSelected (csCurrent <$> cs) circle
      eInsert <- wrapDomEvent (uncheckedCastTo DOM.HTMLElement $ _element_raw svg) (`DOM.on` DOM.mouseUp) $ do
        e <- DOM.event
        x <- DOM.getOffsetX e
        y <- DOM.getOffsetY e
        return (Circle x y 50)

      dSelected <- holdDyn Nothing $ leftmost
        [ Just <$> traceEvent "select" eSelect
        , Nothing <$ eUndo
        , Nothing <$ eRedo
        , Nothing <$ traceEvent "insert" eInsert
        ]

      text "Radius: "
      let selectRadius _ Nothing = 0
          selectRadius s (Just idx) = circleRadius (csCurrent s M.! idx)
      radius <- rangeInput $ def & setValue .~ (selectRadius <$> current cs <@> updated dSelected)
      let eRadius = current (value radius) <@ _rangeInput_mouseup radius
  dynText $ tshow <$> value radius


circle :: MonadWidget t m => Dynamic t Circle -> Dynamic t Bool -> m (Event t ())
circle dCircle dSelected = do
  (c, ()) <- elDynAttrNS' svgNS "circle" (makeAttrs <$> dCircle <*> dSelected) blank
  wrapDomEvent (uncheckedCastTo DOM.HTMLElement $ _element_raw c) (`DOM.on` DOM.mouseUp) DOM.stopPropagation
  where
    makeAttrs (Circle x y r) selected = M.fromList
      [ ("cx", tshow x)
      , ("cy", tshow y)
      , ("r", tshow r)
      , ("stroke", "black")
      , ("fill", if selected then "grey" else "white")
      ]

tshow :: Show t => t -> Text
tshow = T.pack . show

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"


cells :: MonadWidget t m => m ()
cells = blank
