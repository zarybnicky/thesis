module Main
  ( frontend
  , main
  ) where

import Language.Javascript.JSaddle.Warp (run)
import Tapaw.HNPwa (frontend)

main :: IO ()
main = run 3000 frontend
