{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapaw.Storage
  (
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadIO, MonadTrans(lift), ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.Storage (Storage, getItem, setItem)
import Language.Javascript.JSaddle (MonadJSM, liftJSM, fromJSVal, maybeNullOrUndefined)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core hiding (Error, Value)

main :: IO ()
main = run 3000 $ mainWidget $ runKVStoreTPure (UserId 6 =: User) $ do
  pb <- getPostBuild
  putKV ((UserId 5, Just User) <$ pb)
  el "hr" blank
  display =<< getKVAll @User

data User = User
  deriving Show

newtype UserId = UserId
  { unUserId :: Int
  } deriving (Eq, Ord, Show)

type instance StoreKey User = UserId


type family StoreKey k = r | r -> k

class MonadKVStore e t m | m -> t where
  getKV :: Dynamic t (StoreKey e) -> m (Dynamic t (Maybe e))
  getKVAll :: m (Dynamic t (Map (StoreKey e) e))
  putKV :: Event t (StoreKey e, Maybe e) -> m ()
  putKVAll :: Event t (Map (StoreKey e) e) -> m ()


newtype KVStoreT t k m a = KVStoreT
  { unKVStoreT :: ReaderT (Dynamic t (Map (StoreKey k) k)) (EventWriterT t [KVStoreRequest k] m) a
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

instance MonadTrans (KVStoreT t k) where
  lift = KVStoreT . lift . lift

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (KVStoreT t k m) where
  runWithReplace a b = KVStoreT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = KVStoreT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = KVStoreT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance PerformEvent t m => PerformEvent t (KVStoreT t k m) where
  type Performable (KVStoreT t k m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance (Ord (StoreKey k), Monad m, Reflex t) => MonadKVStore k t (KVStoreT t k m) where
  getKVAll = KVStoreT ask
  getKV dId = KVStoreT $ ffor ask (\dMap -> ffor2 dId dMap M.lookup)
  putKV = KVStoreT . tellEvent . fmap ((:[]) . KVUpdateOne)
  putKVAll = KVStoreT . tellEvent . fmap ((:[]) . KVUpdateAll)

data KVStoreRequest e
  = KVUpdateOne (StoreKey e, Maybe e)
  | KVUpdateAll (Map (StoreKey e) e)

runKVStoreReqests :: Ord (StoreKey k) => [KVStoreRequest k] -> Map (StoreKey k) k -> Map (StoreKey k) k
runKVStoreReqests = foldr ((.) . runReq) id
  where
    runReq (KVUpdateOne (k, mv)) = M.alter (const mv) k
    runReq (KVUpdateAll m) = const m


runKVStoreTPure ::
     (PerformEvent t m, MonadHold t m, MonadFix m, Ord (StoreKey k))
  => Map (StoreKey k) k
  -> KVStoreT t k m a
  -> m a
runKVStoreTPure ini f = do
  rec
    dMap <- foldDyn runKVStoreReqests ini eReq
    (a, eReq) <- runEventWriterT (runReaderT (unKVStoreT f) dMap)
  pure a

runKVStoreTStorage ::
     ( PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , Ord (StoreKey k)
     , FromJSONKey (StoreKey k)
     , FromJSON k
     , ToJSONKey (StoreKey k)
     , ToJSON k
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => Storage
  -> Text
  -> KVStoreT t k m a
  -> m a
runKVStoreTStorage store key f = do
  ini <- liftJSM $ (decode . BC.pack . T.unpack =<<) <$> getItem store key
  rec
    dMap <- foldDyn runKVStoreReqests (fromMaybe M.empty ini) eReq
    (a, eReq) <- runEventWriterT (runReaderT (unKVStoreT f) dMap)
  performEvent_ $ setItem store key . T.pack . BC.unpack . encode <$> updated dMap
  pure a
