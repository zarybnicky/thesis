module Main
  ( frontend
  , main
  ) where

import Language.Javascript.JSaddle.Warp (run)
import Project (frontend)

main :: IO ()
main = run 3000 frontend
