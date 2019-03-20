{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tasks.Cells
  ( cells
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace
import Reflex.Dom.Core
import Text.Read (readMaybe)
import Utils (tshow)

data Coord = Coord Int Int
  deriving (Eq, Ord, Show)

cells :: forall t m. MonadWidget t m => m ()
cells = do
  let dExprs :: Dynamic t (Map Coord (Map Coord Double -> Double -> Double))
      dExprs = constDyn $ M.fromList
        [ (Coord 0 0, \_ v -> v)
        , (Coord 0 1, \db _ -> 2 * db M.! Coord 0 0)
        , (Coord 1 0, \db _ -> 2 * db M.! Coord 0 0)
        , (Coord 1 1, \db _ -> db M.! Coord 0 1 + db M.! Coord 1 0)
        ]
      iValues = M.fromList [(Coord x y, fromIntegral (2 * (x + y)) :: Double)
                           | x <- [0..1], y <- [0..1]]
  rec dValues <- foldDyn (uncurry evaluate) iValues $ attachPromptlyDyn dExprs eCell
      eCell <- selectViewListWithKey (pure (Coord 0 0)) dValues (cell dValues)
  blank

evaluate ::
     Map Coord (Map Coord Double -> Double -> Double)
  -> (Coord, Double)
  -> Map Coord Double
  -> Map Coord Double
evaluate exprs elm m = step m (uncurry M.insert elm m)
  where
    step old new =
      traceShow (old, new, old == new) $ if old == new
        then new
        else step new $ traceShowId $ flip M.mapWithKey new $ \k v -> (exprs M.! k) new v

cell ::
     MonadWidget t m
  => Dynamic t (Map Coord Double)
  -> Coord
  -> Dynamic t Double
  -> Dynamic t Bool
  -> m (Event t Double)
cell _ _ dValue _ = do
  initial <- sample (current dValue)
  inp <- inputElement $ def
    & inputElementConfig_setValue .~ (tshow <$> updated dValue)
    & inputElementConfig_initialValue .~ tshow initial
  pure $ fmapMaybe (readMaybe . T.unpack) (_inputElement_input inp)
