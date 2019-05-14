{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Data.Aeson (toJSON)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static (ssMaxAge, staticApp, defaultFileServerSettings)
import Reflex.Dom.Core hiding (Value)
import Tapaw.ServiceWorker.Client
import WaiAppStatic.Types (MaxAge(MaxAgeSeconds))

main :: IO ()
main = debugWrapper $ \refreshMiddleware registerContext -> do
  app <- jsaddleWithAppOr
    defaultConnectionOptions
      (registerContext >> frontend >> syncPoint)
      (refreshMiddleware $ staticApp $ (defaultFileServerSettings "static") {ssMaxAge = MaxAgeSeconds 0})
  runSettings (setPort 3000 . setTimeout 3600 $ defaultSettings) app

frontend :: JSM ()
frontend = mainWidgetWithHead (pure ()) $ runServiceWorkerT "sw.js" (ServiceWorkerOptions $ Just ".") (Just ())$ do
  eReg <- fmap (fmap unServiceWorkerRegistration) . updated <$> getSWRegistration
  _ <- performEvent $ ffor eReg $ \reg -> liftJSM $ jsg ("console" :: Text) ^. js1 ("log" :: Text) reg
  dSub <- getPushSubscription
  let eSub = fmap unPushSubscription <$> updated dSub
  _ <- performEvent $ ffor eSub $ \sub -> liftJSM $ jsg ("console" :: Text) ^. js1 ("log" :: Text) sub
  el "br" blank
  dynText . fmap (T.pack . show . toJSON) =<< getPushPermissionState

  eNotify <- button "Notify"
  showNotification $ ("Test", Nothing) <$ eNotify

  eDoSub <- button "Subscribe"
  key <- vapidKeyToArray "BHV6HuBAGpuVOBxI8Ko5epUlgUGF7YA7PPnMC-pLmhi0quC9EEVRpIsCYa5RG9rxi8LotD5V7iWO43qwBUTqy_0"
  subscribe $ PushSubscriptionOptions True (Just key) <$ gate (isNothing <$> current dSub) eDoSub
  unsubscribe $ gate (isJust <$> current dSub) eDoSub
