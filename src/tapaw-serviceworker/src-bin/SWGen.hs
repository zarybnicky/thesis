{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BC
import Tapaw.ServiceWorker.Gen

main :: IO ()
main = BC.putStrLn $ generateWorker $ ServiceWorker
  { swPrecache = ["/", "/sw.js", "/all.js"]
  , swPush = PushViewAndOpen "http://localhost:3000/"
  , swFetch =
    [ (matchPath PathMatchEnd, StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "sw.js" PathMatchEnd), StaleWhileRevalidate "precache")
    , (matchPath (matchSegment "all.js" PathMatchEnd), StaleWhileRevalidate "precache")
    ]
  }
