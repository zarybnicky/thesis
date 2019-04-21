{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Control.Applicative
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp as JS (run)
import Polysemy
import Polysemy.Fixpoint
import Polysemy.Internal
import Polysemy.Output
import Reflex.Dom.Core

data Store s t (m :: * -> *) a where
  Get :: Store s t m (Dynamic t s)
  Put :: Event t s -> Store s t m ()

get :: forall s t r m. Member (Store s t) r => SemR t r m (Dynamic t s)
get = SemR $ send Get
{-# INLINABLE get #-}

put :: forall t s r m. Member (Store s t) r => Event t s -> SemR t r m ()
put a = SemR $ send (Put a)
{-# INLINABLE put #-}

runStore ::
     forall s t m r a.
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

instance (Member (Lift m) r, NotReady t m) => NotReady t (SemR t r m) where
  notReadyUntil e = SemR $ sendM @m (notReadyUntil e)
  notReady = SemR $ sendM @m notReady

instance (Member (Lift m) r, MonadSample t m) => MonadSample t (SemR t r m) where
  sample b = SemR $ sendM @m (sample b)

instance (Member (Lift m) r, MonadHold t m) => MonadHold t (SemR t r m) where
  hold a e = SemR $ sendM @m (hold a e)
  holdDyn a e = SemR $ sendM @m (holdDyn a e)
  holdIncremental a e = SemR $ sendM @m (holdIncremental a e)
  buildDynamic m e = SemR $ sendM @m (buildDynamic m e)
  headE e = SemR $ sendM @m (headE e)

instance (Member (Lift m) r, PerformEvent t m) => PerformEvent t (SemR t r m) where
  type Performable (SemR t r m) = Performable m
  performEvent e = SemR $ sendM @m (performEvent e)
  performEvent_ e = SemR $ sendM @m (performEvent_ e)

instance (Member (Lift m) r, PostBuild t m) => PostBuild t (SemR t r m) where
  getPostBuild = SemR $ sendM @m getPostBuild

instance (Member (Lift m) r, TriggerEvent t m) => TriggerEvent t (SemR t r m) where
  newTriggerEvent = SemR $ sendM @m newTriggerEvent
  newTriggerEventWithOnComplete = SemR $ sendM @m newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f = SemR $ sendM @m @r (newEventWithLazyTriggerWithOnComplete f)

instance (Reflex t, Adjustable t m) => Adjustable t (SemR t r m) where
  runWithReplace = undefined
  traverseIntMapWithKeyWithAdjust = undefined
  traverseDMapWithKeyWithAdjustWithMove _ _ = undefined

instance (Member (Lift m) r, MonadHold t m, DomBuilder t m) => DomBuilder t (SemR t r m) where
  type DomBuilderSpace (SemR t r m) = DomBuilderSpace m
  inputElement cfg = SemR $ sendM @m (inputElement cfg)
  textAreaElement cfg = SemR $ sendM @m (textAreaElement cfg)
  selectElement = undefined
  textNode cfg = SemR $ sendM @m (textNode cfg)
  commentNode cfg = SemR $ sendM @m (commentNode cfg)
  placeRawElement e = SemR $ sendM @m (placeRawElement e)
  wrapRawElement e cfg = SemR $ sendM @m (wrapRawElement e cfg)
  element e cfg (child :: SemR t r m a) = SemR $ sendM @m $ element e cfg undefined


newtype SemR t r (m :: * -> *) a = SemR
  { unSemR :: Semantic r a
  } deriving (Functor, Applicative, Monad)
type SemRef t r m a = (Member Fixpoint r, Member (Lift m) r) => SemR t r m a

runSemR ::
     forall t m a. MonadFix m
  => SemR t '[ Fixpoint, Lift m] m a
  -> m a
runSemR = runM . runFixpointM runM . unSemR

stripSemR ::
     (Semantic (e ': r) a -> Semantic r a)
  -> SemR t (e ': r) m a
  -> SemR t r m a
stripSemR runner = SemR . runner . unSemR

main :: IO ()
main = JS.run 3000 $ mainWidget go
  where
    go :: forall t m. (Typeable t, MonadWidget t m) => m ()
    go = runSemR $ stripSemR (runStore @Text @t @m "") app

app :: forall t m r. (Member (Store Text t) r, MonadWidget t m) => SemRef t r m ()
app = do
  dynText =<< get @Text
  dText <- inputElement def
  eSave <- button "Save"
  put (current (value dText) <@ eSave)
