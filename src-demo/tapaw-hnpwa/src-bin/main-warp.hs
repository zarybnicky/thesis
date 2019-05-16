{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( frontend
  , main
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static (StaticSettings(..), staticApp, defaultFileServerSettings)
import Tapaw.HNPwa (frontend, serviceWorker, webManifest)
import System.FilePath ((</>))
import Tapaw.ServiceWorker (generateWorker)
import WaiAppStatic.Types (File(..), LookupResult(..), MaxAge(MaxAgeSeconds), fromPiece, toPiece)

main :: IO ()
main = devMain 3000

devMain :: Int -> IO ()
devMain port =
  debugWrapper $ \refreshMiddleware registerContext -> do
    app <-
      jsaddleWithAppOr
        defaultConnectionOptions
        (registerContext >> frontend >> syncPoint)
        (refreshMiddleware staticServer)
    runSettings (setPort port . setTimeout 3600 $ defaultSettings) app

  where
    staticServer = staticApp $ defServer
      { ssMaxAge = MaxAgeSeconds 0
      , ssLookupFile = generateOr (ssLookupFile defServer)
      }
    defServer = defaultFileServerSettings "static"
    generateOr lookupFile ps = case foldl (\fp p -> fp </> T.unpack (fromPiece p)) "/" ps of
      "/sw.js" -> do
        let sw = generateWorker serviceWorker
        pure $ LRFile $ File
          { fileGetSize = fromIntegral $ LBS.length (LBS.fromStrict sw)
          , fileToResponse = \s h -> responseLBS s h (LBS.fromStrict sw)
          , fileName = fromJust (toPiece "sw.js")
          , fileGetHash = pure Nothing
          , fileGetModified = Nothing
          }
      "/site.webmanifest" -> do
        let wm = encode webManifest
        pure $ LRFile $ File
          { fileGetSize = fromIntegral $ LBS.length wm
          , fileToResponse = \s h -> responseLBS s h wm
          , fileName = fromJust (toPiece "site.webmanifest")
          , fileGetHash = pure Nothing
          , fileGetModified = Nothing
          }
      "/index.html" -> pure LRNotFound
      _ -> lookupFile ps
