{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BC
import Tapaw.ServiceWorker

main :: IO ()
main = BC.putStrLn $ generateWorker $ ServiceWorker
  { swPrecache = ["/index.html", "/sw.js", "/all.js"]
  , swPush = PushViewAndOpen "http://localhost:3000/"
  , swFetch =
    [ (matchPath (matchSegment "index.html" PathMatchEnd), StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "sw.js" PathMatchEnd), StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "all.js" PathMatchEnd), StaleWhileRevalidate "precache")
    ]
  }
