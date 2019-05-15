{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types
  ( AppT(..)
  , AppRoute(..)
  , Task(..)
  , TaskFilter(..)
  , runAppT
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Reader (MonadIO, MonadFix, MonadTrans(lift))
import Data.Coerce (coerce)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Window (getLocalStorage)
import GHCJS.DOM.Types (MonadJSM)
import Servant.API ((:>))
import Servant.API.Generic ((:-))
import Tapaw.Servant (App, Loc(..), MonadRouted, RoutedT, runRoutedTHash)
import Tapaw.Storage (MonadKVStore(..), KVStoreT, StoreKey, runKVStoreTStorage)
import Reflex.Dom.Core


data AppRoute r = AppRoute
  { rRoot :: r :- App
  , rAll :: r :- "all" :> App
  , rActive :: r :- "active" :> App
  , rCompleted :: r :- "completed" :> App
  } deriving (Generic)

newtype AppT t m a = AppT
  { unAppT :: RoutedT t AppRoute (KVStoreT t Task m) a
  } deriving newtype
             ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadIO
             , MonadJSM
             , MonadSample t
             , MonadHold t
             , PostBuild t
             , NotReady t
             , DomBuilder t
             , TriggerEvent t
             , MonadRouted AppRoute t
             )

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (AppT t m) where
  runWithReplace a b = AppT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = AppT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = AppT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance PerformEvent t m => PerformEvent t (AppT t m) where
  type Performable (AppT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance (Monad m, Reflex t) => MonadKVStore Task t (AppT t m) where
  getKV = AppT . lift . getKV
  getKVAll = AppT $ lift getKVAll
  putKV = AppT . lift . putKV
  putKVAll = AppT . lift . putKVAll

instance MonadTrans (AppT t) where
  lift = AppT . lift . lift

runAppT ::
     ( PerformEvent t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , MonadHold t m
     , MonadFix m
     )
  => AppT t m a
  -> m a
runAppT f = do
  storage <- getLocalStorage =<< currentWindowUnchecked
  runKVStoreTStorage storage "todosReflex" $ runRoutedTHash (Loc [] []) (unAppT f)


data Task = Task
  { title :: Text
  , completed :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

type instance StoreKey Task = Int

data TaskFilter
  = FilterAll
  | FilterActive
  | FilterCompleted
  deriving Eq
