{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidget $ do
  elAttr "div" ("style" =: "color:blue") $ text "The Reflex app is running!"

  eClick <- button "Click Me"
  n <- foldDyn (+) (0 :: Int) (1 <$ eClick)
  display n
