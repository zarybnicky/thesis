{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( headWidget
  , bodyWidget
  , main
  ) where

import Control.Monad.Reader
import Data.Bool (bool)
import Data.Coerce (coerce)
import qualified Data.Dependent.Sum as DS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BC
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = if True
  then run 3000 $ mainHydrationWidgetWithHead headWidget bodyWidget
  else BC.putStrLn . snd =<< renderStatic bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  el "title" (text "HNPWA")
  elAttr "meta" ("http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width,initial-scale=1") blank
  elAttr "meta" ("charset" =: "utf-8") blank

bodyWidget ::
     ( Prerender js t m
     , DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , MonadFix m
     , PostBuild t m
     )
  => m ()
bodyWidget = do
  _ <- prerender (spinner $ pure True) (text "Started!")
  _ <- runUserT $ do
    pb <- getPostBuild
    display =<< holdDyn Nothing . fmap Just =<< insert (User <$ pb)
    el "hr" blank
    display =<< fetchAll
  pure ()

spinner :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m ()
spinner dShow =
  elDynAttrNS svgNS "svg" (("class" =: "spinner" <>) . (baseAttrs <>) . ("style" =:) . bool "display:none" "" <$> dShow) $
    elDynAttrNS svgNS "circle" (pure circleAttrs) blank
  where
    baseAttrs, circleAttrs :: Map Text Text
    baseAttrs = "width" =: "44px" <> "height" =: "44px" <> "viewBox" =: "0 0 44 44"
    circleAttrs = "class" =: "path" <> "fill" =: "none" <> "cx" =: "22" <>
                   "cy" =: "22" <> "r" =: "20" <> "stroke-width" =: "4" <>
                   "stroke-linecap" =: "round"

svgNS :: Maybe Text
svgNS = Just "http://www.w3.org/2000/svg"

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
        fmap (\(_ DS.:=> UserReq (i, u)) -> M.alter (const u) i) .
        requesterDataToList
  eRes <- performEvent $ ffor eReq $ traverseRequesterData $ \(UserReq x) -> pure (UserRes x)
  pure a
