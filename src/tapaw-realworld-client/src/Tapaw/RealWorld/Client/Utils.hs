{-# LANGUAGE OverloadedStrings #-}

module Tapaw.RealWorld.Client.Utils
  ( tshow
  , (<!>)
  , (=?)
  , (=!)
  ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core (Dynamic, Reflex, (=:))

tshow :: Show a => a -> Text
tshow = T.pack . show

infixr 4 <!>
infixr 7 =?, =!

(<!>) :: (Semigroup a, Functor f) => a -> f a -> f a
a <!> b = (a <>) <$> b

(=?) :: Ord k => k -> Maybe v -> Map k v
k =? mVal = maybe mempty (k =:) mVal

(=!) :: Reflex t => k -> Dynamic t v -> Dynamic t (Map k v)
k =! dVal = (k =:) <$> dVal
