{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Tasks.CircleDrawer
  ( circleDrawer
  ) where

import Control.Lens
import Data.Generics.Product (field)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.MouseEvent as DOM
import GHCJS.DOM.Types (uncheckedCastTo)
import Reflex.Dom.Core

import Utils (buttonMaybe, selectViewListWithMaybeKey_, tshow)


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

circleDrawer :: MonadWidget t m => m ()
circleDrawer = do
  let initCs = CircleState [] [] M.empty 0
  rec cs <- foldDyn ($) initCs $ leftmost
        [ undo <$> eUndo
        , redo <$> eRedo
        , newAction . CircleCreate <$> eInsert
        , fmapMaybe (fmap newAction) $ (\idx r -> flip CircleAdjust r <$> idx) <$> current dSelected <@> eRadius
        ]
      eUndo <- buttonMaybe ((^? field @"csUndo" . _head) <$> cs) "Undo"
      eRedo <- buttonMaybe ((^? field @"csRedo" . _head) <$> cs) "Redo"

      (svg, eSelect) <- elDynAttrNS' svgNS "svg" (pure $ "style" =: "display:block;width:600px;height:300px") $ do
        elDynAttrNS svgNS "rect" (pure $ "width" =: "600" <> "height" =: "300" <> "stroke" =: "black" <> "fill" =: "none") blank
        selectViewListWithMaybeKey_ dSelected (csCurrent <$> cs) circle
      let svg' = uncheckedCastTo DOM.HTMLElement (_element_raw svg)
      eInsert <- wrapDomEvent svg' (`DOM.on` DOM.mouseUp) $ do
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
  let e = uncheckedCastTo DOM.HTMLElement $ _element_raw c
  wrapDomEvent e (`DOM.on` DOM.mouseUp) DOM.stopPropagation
  where
    makeAttrs (Circle x y r) selected = M.fromList
      [ ("cx", tshow x)
      , ("cy", tshow y)
      , ("r", tshow r)
      , ("stroke", "black")
      , ("fill", if selected then "grey" else "white")
      ]

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"
