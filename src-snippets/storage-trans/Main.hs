{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Coerce (coerce)
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

class MonadUser t m | m -> t where
  fetchAll :: m (Dynamic t (Map Int User))
  fetchOne :: Dynamic t Int -> m (Dynamic t (Maybe User))
  insert :: Event t User -> m (Event t Int)
  update :: Event t (Int, User) -> m ()
  delete :: Event t Int -> m ()

data AppRequest a where
  UserReq :: (Int, Maybe User) -> AppRequest User

data AppResponse a where
  UserRes :: (Int, Maybe User) -> AppResponse User

instance
  ( MonadReader (Dynamic t (Map Int User)) m
  , Requester t m
  , Request m ~ AppRequest
  , Response m ~ AppResponse
  ) => MonadUser t m where
  fetchAll = ask
  fetchOne dId = ffor ask (\dMap -> ffor2 dId dMap M.lookup)
  insert eUser = do
    dMap <- ask
    let dNextId = foldr max 0 . M.keys <$> dMap
    eRes <- requesting (attachWith (\i u -> UserReq (i, Just u)) (current dNextId) eUser)
    pure $ ffor eRes (\(UserRes (x, _)) -> x)
  update eBoth = requesting_ $ ffor eBoth (\(i, u) -> UserReq (i, Just u))
  delete eId = requesting_ . ffor eId $ \i -> UserReq (i, Nothing)

newtype UserT t m a = UserT
  { unUserT :: ReaderT (Dynamic t (Map Int User)) (RequesterT t AppRequest AppResponse m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Dynamic t (Map Int User))
             , PostBuild t
             , NotReady t
             , DomBuilder t
             , MonadSample t
             , MonadHold t
             )

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (UserT t m) where
  runWithReplace a b = UserT $ runWithReplace (coerce a) (coerceEvent b)
  traverseIntMapWithKeyWithAdjust a b c = UserT $ traverseIntMapWithKeyWithAdjust (coerce a) b c
  traverseDMapWithKeyWithAdjustWithMove a b c = UserT $ traverseDMapWithKeyWithAdjustWithMove (coerce a) b c

instance (Monad m, Reflex t) => Requester t (UserT t m) where
  type Request (UserT t m) = AppRequest
  type Response (UserT t m) = AppResponse
  requesting = UserT . requesting
  requesting_ = UserT . requesting_

runUserT :: (PerformEvent t m, MonadHold t m, MonadFix m) => UserT t m a -> m a
runUserT f = mdo
  dMap <- foldDyn ($) M.empty eUpdate
  (a, eReq) <- flip runRequesterT eRes . flip runReaderT dMap $ unUserT f
  let eUpdate = ffor eReq $
        foldr (.) id .
        fmap (\(_ :=> UserReq (i, u)) -> M.alter (const u) i) .
        requesterDataToList
  eRes <- performEvent $ ffor eReq $ traverseRequesterData $ \(UserReq x) -> pure (UserRes x)
  pure a
