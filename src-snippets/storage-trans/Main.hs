{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Dependent.Sum (DSum((:=>)))
import Data.Map (Map)
import qualified Data.Map as M
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidget $ do
  _ <- runUserT $ do
    pb <- getPostBuild
    display =<< holdDyn Nothing . fmap Just =<< insert (User <$ pb)
    el "hr" blank
    display =<< fetchAll
  pure ()

data User = User
  deriving Show

newtype UserId = UserId
  { unUserId :: Int
  } deriving (Eq, Ord, Show, Default)

type family StoreKey k = r | r -> k
type instance StoreKey User = UserId

class MonadStorage k t m | m -> t where
  fetchAll :: m (Dynamic t (Map (StoreKey k) k))
  fetchOne :: Dynamic t (StoreKey k) -> m (Dynamic t (Maybe k))
  insert :: Event t k -> m (Event t (StoreKey k))
  update :: Event t ((StoreKey k), k) -> m ()
  delete :: Event t (StoreKey k) -> m ()

data StorageRequest k a where
  StoreReq :: (Maybe (StoreKey k), Maybe k) -> StorageRequest k k
data StorageResponse k a where
  StoreRes :: (StoreKey k, Maybe k) -> StorageResponse k k

instance
  ( Ord (StoreKey k)
  , MonadReader (Dynamic t (Map (StoreKey k) k)) m
  , Requester t m
  , Request m ~ StorageRequest k
  , Response m ~ StorageResponse k
  ) => MonadStorage k t m where
  fetchAll = ask
  fetchOne dId = ffor ask (\dMap -> ffor2 dId dMap M.lookup)
  insert eUser = do
    dMap <- ask
    eRes <- requesting ((\u -> StoreReq (Nothing, Just u)) <$> eUser)
    pure $ ffor eRes (\(StoreRes (x, _)) -> x)
  update eBoth = requesting_ $ ffor eBoth (\(i, u) -> StoreReq (Just i, Just u))
  delete eId = requesting_ . ffor eId $ \i -> StoreReq (Just i, Nothing)

newtype StorageT k t m a = UserT
  { unUserT :: ReaderT (Dynamic t (Map (StoreKey k) k)) (RequesterT t (StorageRequest k) (StorageResponse k) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , PostBuild t
             , NotReady t
             , DomBuilder t
             , MonadSample t
             , MonadHold t
             )
deriving instance (Monad m, key ~ StoreKey k) => MonadReader (Dynamic t (Map key k)) (StorageT k t m)


instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (StorageT k t m) where
  runWithReplace a b = UserT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = UserT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = UserT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance (Monad m, Reflex t) => Requester t (StorageT k t m) where
  type Request (StorageT k t m) = StorageRequest k
  type Response (StorageT k t m) = StorageResponse k
  requesting = UserT . requesting
  requesting_ = UserT . requesting_

runUserT :: (PerformEvent t m, MonadHold t m, MonadFix m, Ord (StoreKey k)) => StorageT k t m a -> m a
runUserT f = mdo
  dMap <- foldDyn ($) M.empty eUpdate
  (a, eReq) <- flip runRequesterT eRes . flip runReaderT dMap $ unUserT f
  let eUpdate = ffor eReq $
        foldr (.) id .
        fmap (\(_ :=> StoreReq (i, u)) -> M.alter (const u) i) .
        requesterDataToList
  eRes <- performEvent $ ffor eReq $ traverseRequesterData pure
  pure a
