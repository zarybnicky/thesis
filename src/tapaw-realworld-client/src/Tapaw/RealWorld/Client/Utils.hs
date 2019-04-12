{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapaw.RealWorld.Client.Utils
  ( tshow
  , (<!>)
  , (=?)
  , (=!)
  , form
  , formAttr
  , formDynAttr'
  ) where

import Control.Lens ((%~))
import Data.Map (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

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


form :: (PostBuild t m, DomBuilder t m) => m (Dynamic t a) -> m (Event t (), Dynamic t a)
form = formDynAttr' (pure mempty)

formAttr :: (PostBuild t m, DomBuilder t m) => Map AttributeName Text -> m (Dynamic t a) -> m (Event t (), Dynamic t a)
formAttr attrs = formDynAttr' (pure attrs)

-- form :: (PostBuild t m, DomBuilder t m) => m (Dynamic t a) -> m (Event t a)
-- form = (\(a, b) -> current b <@ a) <$> formDynAttr' (pure mempty)

-- formAttr :: (PostBuild t m, DomBuilder t m) => Map AttributeName Text -> m (Dynamic t a) -> m (Event t a)
-- formAttr attrs x = (\(a, b) -> current b <@ a) <$> formDynAttr' (pure attrs) x

formDynAttr' ::
     forall t m a. (PostBuild t m, DomBuilder t m)
  => Dynamic t (Map AttributeName Text)
  -> m a
  -> m (Event t (), a)
formDynAttr' attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  (e, ch) <- element "form" ((def :: ElementConfig EventResult t (DomBuilderSpace m))
    & modifyAttributes .~ modifyAttrs
    & elementConfig_eventSpec %~
        addEventSpecFlags
        (Proxy :: Proxy (DomBuilderSpace m))
        Submit
        (const preventDefault)) child
  pure (domEvent Submit e, ch)
