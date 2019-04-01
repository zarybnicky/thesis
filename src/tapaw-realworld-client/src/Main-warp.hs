module Main
  ( frontend
  , main
  , staticServer
  ) where

import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static (ssMaxAge, staticApp, defaultFileServerSettings)
import Tapaw.RealWorld.Client (frontend)
import WaiAppStatic.Types (MaxAge(MaxAgeSeconds))

main :: IO ()
main = devMain 8000

devMain :: Int -> IO ()
devMain port =
  debugWrapper $ \refreshMiddleware registerContext -> do
    app <-
      jsaddleWithAppOr
        defaultConnectionOptions
        (registerContext >> frontend >> syncPoint)
        (refreshMiddleware staticServer)
    runSettings (setPort port . setTimeout 3600 $ defaultSettings) app

staticServer :: Application
staticServer =
  staticApp
    ((defaultFileServerSettings "static")
       {ssMaxAge = MaxAgeSeconds 0})
