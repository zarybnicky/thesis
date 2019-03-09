{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T
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
  n <- foldDyn (+) (0 :: Int) (1 <$ eClick)
  text "Clicks: "
  display n

tempConverter :: MonadWidget t m => m ()
tempConverter = do
  rec celsius <- textInput $ def & setValue .~ fmapMaybe (withIntMaybe f2c) (_textInput_input fahrenheit)
      text " Celsius = "
      fahrenheit <- textInput $ def & setValue .~ fmapMaybe (withIntMaybe c2f) (_textInput_input celsius)
  text " Fahrenheit"
  where
    f2c, c2f :: Double -> Double
    f2c x = (x - 32) * 5 / 9
    c2f x = x * 9 / 5 + 32
    withIntMaybe f = fmap (T.pack . show . f) . readMaybe . T.unpack

data FlightType = OneWay | Return

flightBooker :: MonadWidget t m => m ()
flightBooker = do
  blank

timer :: MonadWidget t m => m ()
timer = blank

crud :: MonadWidget t m => m ()
crud = blank

circleDrawer :: MonadWidget t m => m ()
circleDrawer = blank

cells :: MonadWidget t m => m ()
cells = blank
