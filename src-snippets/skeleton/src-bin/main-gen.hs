module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Project (serviceWorker, webManifest)
import Tapaw.ServiceWorker.Gen (generateWorker)

main :: IO ()
main = do
  BLC.writeFile "site.webmanifest" (encode webManifest)
  BLC.writeFile "sw.js" (BLC.fromStrict $ generateWorker serviceWorker)
