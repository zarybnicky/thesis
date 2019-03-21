{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

import Tasks.Counter (counter)
import Tasks.TempConverter (tempConverter)
import Tasks.FlightBooker (flightBooker)
import Tasks.Timer (timer)
import Tasks.Crud (crud)
import Tasks.CircleDrawer (circleDrawer)
import Tasks.Cells (cells)


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
  , ".cell { position: relative; }"
  , ".cell > input { position: absolute; top: 0; right: 0; bottom: 0; left: 0; }"
  ]
