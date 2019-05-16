module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Tapaw.HNPwa (serviceWorker, webManifest)
import Tapaw.ServiceWorker (generateWorker)

main :: IO ()
main = do
  BLC.writeFile "site.webmanifest" (encode webManifest)
  BLC.writeFile "sw.js" (BLC.fromStrict $ generateWorker serviceWorker)
