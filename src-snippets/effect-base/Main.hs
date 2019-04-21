{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp as JS (run)
import Polysemy
import Polysemy.Fixpoint
import Polysemy.Internal (send)
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


instance Applicative (SemR t r m)
instance Monad (SemR t r m) where
instance MonadTrans (SemR t r) where
instance (Reflex t, Adjustable t m) => Adjustable t (SemR t r m) where
instance NotReady t m => NotReady t (SemR t r m) where
instance (MonadHold t m, DomBuilder t m) => DomBuilder t (SemR t r m) where
  type DomBuilderSpace (SemR t r m) = DomBuilderSpace m
  element = undefined
  inputElement = undefined
  textAreaElement = undefined
  selectElement = undefined
instance PerformEvent t m => PerformEvent t (SemR t r m) where
  type Performable (SemR t r m) = Performable m
instance PostBuild t m => PostBuild t (SemR t r m) where
instance TriggerEvent t m => TriggerEvent t (SemR t r m) where
--instance DomSpace (DomBuilderSpace (SemR t m r)) where

newtype SemR t r m a = SemR
  { unSemR :: (Member Fixpoint r, Member (Lift m) r) => Semantic r a
  } deriving (Functor)

runSemR ::
     forall t m a. MonadFix m
  => SemR t '[ Fixpoint, Lift m] m a
  -> m a
runSemR = runM . runFixpointM runM . unSemR

stripSemR ::
     (Member (Lift m) (e : r), Member Fixpoint (e : r))
  => (Semantic (e ': r) a -> Semantic r a)
  -> SemR t (e ': r) m a
  -> SemR t r m a
stripSemR runner = SemR . runner . unSemR

main :: IO ()
main = JS.run 3000 $ mainWidget go
  where
    go :: forall t m. (Typeable t, MonadWidget t m) => m ()
    go = runSemR $ stripSemR (runStore @Text @t @m "") app

app :: forall t m r. (Member (Store Text t) r, MonadWidget t m) => SemR t r m ()
app = do
  dynText =<< get @Text
  dText <- inputElement def
  eSave <- button "Save"
  put (current (value dText) <@ eSave)
