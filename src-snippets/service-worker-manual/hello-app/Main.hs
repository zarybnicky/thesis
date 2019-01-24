{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle (eval, fun, js, jsg, jsf, liftJSM, toJSVal)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidget $ do
  el "div" $ elAttr "span" ("style" =: "color:blue") $ text "Text inside span"
  el "dl" $ do
    dtdd "dt dd tags" $ text "Here goes the description"
    dtdd "Reflex" $ do
      text "Haskell + awesome FRP!"
      el "br" blank
      elAttr "a" ("href" =: "http://reflexfrp.org") (text "Reflex-FRP")

  (eMessage, onMessage) <- newTriggerEvent
  msgText <- liftJSM $ toJSVal ("message" :: String)
  callback <- liftJSM $ toJSVal $ fun $ \_ _ [message] -> liftIO $ onMessage $ message ^. js ("data" :: String)
  _ <- liftJSM $ jsg ("navigator" :: String) ^. js ("serviceWorker" :: String) . jsf ("addEventListener" :: String)
    [ msgText
    , callback
    ]

  performEvent_ $
    ffor eMessage
      (\val -> () <$ liftJSM (eval ("console" :: String) ^. jsf ("log" :: String) [val]))
