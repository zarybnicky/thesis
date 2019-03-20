{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.Timer
  ( timer
  ) where

import Data.Bool (bool)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Reflex.Dom.Core

import Utils (tshow)


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

roundDouble :: Float -> Float
roundDouble = (/ 10) . fromIntegral . (round :: Float -> Int) . (* 10)
