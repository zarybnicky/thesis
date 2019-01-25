{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Language.Javascript.JSaddle (valToText, fun, js, jsg, jsf, liftJSM, toJSVal)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidget $ do

  (eMessage, onMessage) <- newTriggerEvent

  -- Listen to messages from the service worker
  -- navigator.serviceWorker.addEventListener("message", (e) => {
  --   onMessage(e.data);
  -- });
  liftJSM $ do
    serviceWorker <- jsg ("navigator" :: Text) ^. js ("serviceWorker" :: Text)
    callback <- toJSVal $ fun $ \_ _ [message] -> do
      msgData <- valToText (message ^. js ("data" :: Text))
      liftIO $ onMessage msgData
    msgText <- toJSVal ("message" :: Text)
    _ <- serviceWorker ^. jsf ("addEventListener" :: Text) [msgText, callback]
    return ()

  el "div" $ elAttr "span" ("style" =: "color:blue") $
    text "Reflex script running!"

  -- Display a list of messages from the worker
  el "div" $ do
    msgs <- foldDyn (:) [] eMessage
    _ <- el "ul" $ simpleList msgs $ el "li" . dynText
    return ()
