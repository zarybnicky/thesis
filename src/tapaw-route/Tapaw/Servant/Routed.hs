{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapaw.Servant.Routed
  ( MonadRouted(..)
  , RoutedT(..)
  , SomeRoute(..)
  , someRouteToLoc
  , runRouter
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadIO, MonadTrans(lift), MonadReader(..), ReaderT(..), runReaderT)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core
import Tapaw.Servant.AsApp (AsApp, HasApp, MkApp, route)
import Tapaw.Servant.AsAppLink (GatherLinkArgs, HasAppLink, safeAppLink)
import Tapaw.Servant.TupleProduct (TupleProductOf)
import Tapaw.Servant.Types (Err, Loc(..))
import Servant.API (IsElem)
import Servant.API.Generic (AsApi, GenericServant, ToServant, ToServantApi, genericApi, toServant)

class MonadRouted (r :: * -> *) t m | m -> r t where
  setRoute :: Event t (SomeRoute r) -> m ()
  default setRoute :: (m ~ f m', Monad m', MonadTrans f, MonadRouted r t m') => Event t (SomeRoute r) -> m ()
  setRoute = lift . setRoute

  getRoute :: m (Dynamic t (Either Err Loc))
  default getRoute :: (m ~ f m', Monad m', MonadTrans f, MonadRouted r t m') => m (Dynamic t (Either Err Loc))
  getRoute = lift getRoute

newtype RoutedT t (r :: * -> *) m a = RoutedT
  { unRoutedT :: ReaderT (Dynamic t (Either Err Loc)) (EventWriterT t Loc m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadIO
#ifndef ghcjs_HOST_OS
             , MonadJSM
#endif
             , MonadSample t
             , MonadHold t
             , PostBuild t
             , NotReady t
             , DomBuilder t
             , TriggerEvent t
             , Prerender js t
             )

instance MonadTrans (RoutedT t k) where
  lift = RoutedT . lift . lift

instance MonadReader x m => MonadReader x (RoutedT r t m) where
  ask = lift ask
  local f (RoutedT a) = RoutedT $ ReaderT (mapEventWriterT (local f) . runReaderT a)

instance EventWriter t w m => EventWriter t w (RoutedT t r m) where
  tellEvent = lift . tellEvent

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (RoutedT t k m) where
  runWithReplace a b = RoutedT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = RoutedT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = RoutedT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance PerformEvent t m => PerformEvent t (RoutedT t k m) where
  type Performable (RoutedT t k m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance ( GenericServant r AsApi
         , Monad m
         , DomBuilder t m
         , MonadHold t m
         , PostBuild t m
         , Reflex t
         ) =>
         MonadRouted r t (RoutedT t r m) where
  setRoute e = RoutedT $ tellEvent (someRouteToLoc <$> e)
  getRoute = RoutedT ask

runRouter ::
     forall r t m a.
     ( DomBuilder t m
     , GenericServant r (AsApp (m a))
     , ToServant r (AsApp (m a)) ~ MkApp (ToServantApi r) (m a)
     , HasApp (ToServantApi r)
     , MonadRouted r t m
     , MonadHold t m
     )
  => r (AsApp (m a))
  -> (Err -> m a)
  -> m (Dynamic t a)
runRouter ws onError = do
  dUrl <- getRoute
  url0 <- sample (current dUrl)
  widgetHold (run url0) (run <$> updated dUrl)
  where
    run = either onError id . (route (Proxy @(ToServantApi r)) (toServant ws) =<<)

data SomeRoute api where
  SomeRoute
    :: forall e api. (HasAppLink e, IsElem e (ToServantApi api))
    => (api AsApi -> e) -> TupleProductOf (GatherLinkArgs e)
    -> SomeRoute api

someRouteToLoc :: GenericServant r AsApi => SomeRoute r -> Loc
someRouteToLoc (SomeRoute (_ :: r AsApi -> e) args) =
  safeAppLink (genericApi (Proxy @r)) (Proxy @e) (Loc [] []) args
