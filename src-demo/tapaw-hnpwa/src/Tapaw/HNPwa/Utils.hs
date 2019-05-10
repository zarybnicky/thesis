{-# LANGUAGE OverloadedStrings #-}

module Tapaw.HNPwa.Utils
  ( getUrlHost
  , pluralize
  , tshow
  , (<!>)
  , (=?)
  , (=!)
  ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core (Dynamic, Reflex, (=:))


getUrlHost :: Text -> Text
getUrlHost = T.intercalate "/"
  . take 1
  . filter (\x -> not (T.null x) && x /= "www" && x /= "https:" && x /= "http:")
  . T.split (== '/')

pluralize :: Int -> Text -> Text
pluralize 1 x = "1" <> x <> "s"
pluralize n x = tshow n <> x <> "s"

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
