{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Data.Text (Text)
import Language.Javascript.JSaddle.Warp as JS (run)
import Polysemy
import Polysemy.Fixpoint
import Polysemy.Output
import Reflex.Dom.Core

data Store s t (m :: * -> *) a where
  Get :: Store s t m (Dynamic t s)
  Put :: Event t s -> Store s t m ()

makeSemantic ''Store

runStore ::
     forall m s t r a.
     ( Typeable s
     , Typeable t
     , Reflex t
     , MonadHold t m
     , Member Fixpoint r
     , Member (Lift m) r
     )
  => s
  -> Semantic (Store s t ': r) a
  -> Semantic r a
runStore s0 f = mdo
  s <- sendM @m $ holdDyn s0 (leftmost es)
  (es, a) <- runFoldMapOutput (:[]) $ reinterpret (\case
    Get -> pure s
    Put (e :: Event t s) -> output e) f
  pure a

main :: IO ()
main = JS.run 3000 $ mainWidget widget

widget :: forall t m. (Typeable t, MonadWidget t m) => m ()
widget = runM $ runFixpointM runM $ runStore @m @Text "" $ do
  sendM @m . dynText =<< get
  dText <- sendM @m $ textInput def
  eSave <- sendM @m $ button "Save"
  put (current (value dText) <@ eSave)
