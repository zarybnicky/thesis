{-# LANGUAGE OverloadedStrings #-}

module Project
  ( frontend
  , serviceWorker
  , webManifest
  ) where

import Data.Aeson (toJSON)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import Tapaw.ServiceWorker
import Tapaw.ServiceWorker.Client
import Tapaw.WebManifest (WebManifest(..), emptyManifest)

serviceWorker :: ServiceWorker ()
serviceWorker = ServiceWorker
  { swPrecache = ["/", "/sw.js", "/all.js"]
  , swPush = PushViewAndOpen "http://localhost:3000/"
  , swFetch =
    [ (matchPath PathMatchEnd, StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "sw.js" PathMatchEnd), StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "all.js" PathMatchEnd), StaleWhileRevalidate "precache")
    ]
  }

webManifest :: WebManifest
webManifest = emptyManifest

frontend :: JSM ()
frontend = mainWidget $ runServiceWorkerT "sw.js" (ServiceWorkerOptions $ Just ".") (Just ()) $ do
  dSub <- getPushSubscription
  dynText $ maybe "not subscribed" (const "subscribed") <$> dSub
  el "br" blank
  dState <- getPushPermissionState
  dynText $ ("Notifications: " <>) . T.pack . show . toJSON <$> dState
  el "br" blank
  eNotify <- buttonMaybe (stateToMaybe <$> dState) "Notify"
  showNotification $ ("Test", Nothing) <$ eNotify

  eDoSub <- button "Subscribe"
  key <- vapidKeyToArray "BHV6HuBAGpuVOBxI8Ko5epUlgUGF7YA7PPnMC-pLmhi0quC9EEVRpIsCYa5RG9rxi8LotD5V7iWO43qwBUTqy_0"
  subscribe $ PushSubscriptionOptions True (Just key) <$ gate (isNothing <$> current dSub) eDoSub
  unsubscribe $ gate (isJust <$> current dSub) eDoSub
  pure ()

stateToMaybe :: PermissionState -> Maybe ()
stateToMaybe PermissionGranted = Just ()
stateToMaybe _ = Nothing

buttonMaybe :: (Reflex t, DomBuilder t m, PostBuild t m) => Dynamic t (Maybe a) -> Text -> m (Event t a)
buttonMaybe dDisabled txt = do
  let attrs = maybe ("disabled" =: "disabled") (const mempty) <$> dDisabled
  (e, ()) <- elDynAttr' "button" attrs (text txt)
  pure . fmapMaybe id $ current dDisabled <@ domEvent Click e
