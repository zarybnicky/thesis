{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian, parseTimeM)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core
import Text.Read (readMaybe)

main :: IO ()
main = run 3000 $ mainWidgetWithCss css $
  blank

css :: ByteString
css = mconcat
  [ ""
  ]
