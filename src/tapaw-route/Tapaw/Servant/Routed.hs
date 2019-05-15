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
import Control.Monad.Reader (MonadIO, MonadTrans(lift), ReaderT, ask)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core
import Tapaw.Servant.AsApp (HasApp, MkApp, route)
import Tapaw.Servant.AsAppLink (GatherLinkArgs, HasAppLink, safeAppLink)
import Tapaw.Servant.TupleProduct (TupleProductOf)
import Tapaw.Servant.Types (Err, Loc(..))
import Servant.API (IsElem)
import Servant.API.Generic (AsApi, GenericServant, ToServantApi, genericApi)

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
             )

instance MonadTrans (RoutedT t k) where
  lift = RoutedT . lift . lift

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
     forall r t m.
     (DomBuilder t m, PostBuild t m, HasApp (ToServantApi r), MonadRouted r t m)
  => MkApp (ToServantApi r) m
  -> (Err -> m ())
  -> m ()
runRouter ws onError = do
  dUrl <- getRoute
  _ <- dyn $ either onError id . (route (Proxy @(ToServantApi r)) ws =<<) <$> dUrl
  pure ()

data SomeRoute api where
  SomeRoute
    :: forall e api. (HasAppLink e, IsElem e (ToServantApi api))
    => (api AsApi -> e, TupleProductOf (GatherLinkArgs e))
    -> SomeRoute api

someRouteToLoc :: GenericServant r AsApi => SomeRoute r -> Loc
someRouteToLoc (SomeRoute (_ :: r AsApi -> e, args)) =
  safeAppLink (genericApi (Proxy @r)) (Proxy @e) (Loc [] []) args
