{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapaw.RealWorld.Client.Types
  ( AppState(..)
  , AppStateM
  , AppRoute(..)
  , Route(..)
  , getApi
  , AppT(..)
  , runAppT
  ) where

import Control.Monad.Reader
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core hiding (Client, link)
import Servant.API
import Servant.API.Generic
import Servant.Reflex (BaseUrl, Client, client)
import Tapaw.RealWorld.API (ConduitAPI)
import Tapaw.RealWorld.Types (User)
import Tapaw.Servant

data AppState t = AppState
  { stateNow :: Dynamic t UTCTime
  , stateBaseUrl :: Dynamic t BaseUrl
  , stateUser :: Dynamic t (Maybe User)
  , stateRoute :: Demux t Route
  }

type AppStateM t m
   = ( MonadReader (AppState t) m
     , MonadRouted AppRoute t m
     , EventWriter t (Maybe User) m
     , TriggerEvent t m
     , PerformEvent t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )

getApi :: forall t m a. AppStateM t m => m (Client t m ConduitAPI a)
getApi = client (Proxy @ConduitAPI) (Proxy @m) (Proxy @a) <$> asks stateBaseUrl

data AppRoute r = AppRoute
  { rHome :: r :- App
  , rLogin :: r :- "login" :> App
  , rRegister :: r :- "register" :> App
  , rSettings :: r :- "settings" :> App
  , rEditor :: r :- "editor" :> Capture "slug" (Maybe Text) :> App
  , rArticle :: r :- "article" :> Capture "slug" Text :> App
  , rFavorites :: r :- "profile" :> "favorites" :> Capture "slug" Text :> App
  , rProfile :: r :- "profile" :> Capture "slug" Text :> App
  } deriving (Generic)

data Route
  = RouteHome
  | RouteLogin
  | RouteRegister
  | RouteSettings
  | RouteEditor (Maybe Text)
  | RouteArticle Text
  | RouteProfile Text
  | RouteProfileFavorites Text
  deriving (Eq, Ord, Show, Generic)


newtype AppT t m a = AppT
  { unAppT :: RoutedT t AppRoute (ReaderT (AppState t) m) a
  } deriving newtype
             ( Functor
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
             , MonadRouted AppRoute t
             , MonadReader (AppState t)
             )

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (AppT t m) where
  runWithReplace a b = AppT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = AppT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = AppT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance PerformEvent t m => PerformEvent t (AppT t m) where
  type Performable (AppT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadTrans (AppT t) where
  lift = AppT . lift . lift

instance EventWriter t w m => EventWriter t w (AppT t m) where
  tellEvent = lift . tellEvent

runAppT ::
     ( PerformEvent t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     , MonadHold t m
     , MonadFix m
     )
  => AppState t
  -> AppT t m a
  -> m a
runAppT s f = do
  url0 <- fromRight undefined <$> getInitialRouteHistory
  runReaderT (runRoutedTHistory url0 (unAppT f)) s
