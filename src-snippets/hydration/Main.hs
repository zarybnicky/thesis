{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BC
import Data.Map (Map)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = if True
  then run 3000 $ mainHydrationWidgetWithHead headWidget bodyWidget
  else BC.putStrLn . snd =<< renderStatic bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" (text "App")
  elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width,initial-scale=1") blank
  elAttr "meta" ("charset" =: "utf-8") blank

bodyWidget :: (Prerender js t m, DomBuilder t m, PostBuild t m) => m ()
bodyWidget = do
  _ <- prerender (spinner $ pure True) (text "Started!")
  pure ()

spinner :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m ()
spinner dShow =
  elDynAttrNS svgNS "svg" ((baseAttrs <>) . ("style" =:) . bool "display:none" "" <$> dShow) $
    elDynAttrNS svgNS "circle" (pure circleAttrs) blank
  where
    baseAttrs, circleAttrs :: Map Text Text
    baseAttrs = "width" =: "44px" <> "height" =: "44px" <>
                "viewBox" =: "0 0 44 44" <> "class" =: "spinner"
    circleAttrs = "class" =: "path" <> "fill" =: "none" <> "cx" =: "22" <>
                   "cy" =: "22" <> "r" =: "20" <> "stroke-width" =: "4" <>
                   "stroke-linecap" =: "round"

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"
