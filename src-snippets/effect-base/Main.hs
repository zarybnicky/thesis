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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import qualified Control.Exception as X
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import qualified Data.IntSet as IS
import Data.IntMap (IntMap)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp as JS (run)
import Polysemy
import Polysemy.Fixpoint
import Polysemy.Internal
import Polysemy.Internal.Tactics
import Polysemy.Resource
import Polysemy.Output
import Reflex.Dom.Core

data Store s (t :: *) m a where
  Get :: Store s t m (Dynamic t s)
  Put :: Event t s -> Store s t m ()
makeSemantic ''Store

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

newtype SemR t (m :: * -> *) r a = SemR
  { unSemR :: Semantic r a
  } deriving (Functor, Applicative, Monad)

instance (Member (Lift m) r, NotReady t m) => NotReady t (SemR t m r) where
  notReadyUntil e = SemR $ sendM @m (notReadyUntil e)
  notReady = SemR $ sendM @m notReady

instance (Member (Lift m) r, MonadSample t m) => MonadSample t (SemR t m r) where
  sample b = SemR $ sendM @m (sample b)

instance (Member (Lift m) r, MonadHold t m) => MonadHold t (SemR t m r) where
  hold a e = SemR $ sendM @m (hold a e)
  holdDyn a e = SemR $ sendM @m (holdDyn a e)
  holdIncremental a e = SemR $ sendM @m (holdIncremental a e)
  buildDynamic m e = SemR $ sendM @m (buildDynamic m e)
  headE e = SemR $ sendM @m (headE e)

instance (Member (Lift m) r, PerformEvent t m) => PerformEvent t (SemR t m r) where
  type Performable (SemR t m r) = Performable m
  performEvent e = SemR $ sendM @m (performEvent e)
  performEvent_ e = SemR $ sendM @m (performEvent_ e)

instance (Member (Lift m) r, PostBuild t m) => PostBuild t (SemR t m r) where
  getPostBuild = SemR $ sendM @m getPostBuild

instance (Member (Lift m) r, TriggerEvent t m) => TriggerEvent t (SemR t m r) where
  newTriggerEvent = SemR $ sendM @m newTriggerEvent
  newTriggerEventWithOnComplete = SemR $ sendM @m newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f = SemR $ sendM @m @r (newEventWithLazyTriggerWithOnComplete f)

data DomBuilderEff (t :: *) (m :: * -> *) r a where
  SelectElementE :: SelectElementConfig er t (DomBuilderSpace m) -> r a -> DomBuilderEff t m r (SelectElement er (DomBuilderSpace m) t, a)
  ElementE :: Text -> ElementConfig er t (DomBuilderSpace m) -> r a -> DomBuilderEff t m r (Element er (DomBuilderSpace m) t, a)
  RunWithReplaceE :: r a -> Event t (r b) -> DomBuilderEff t m r (a, Event t b)
  TraverseIntMapWithKeyWithAdjustE :: (IS.Key -> v -> r v') -> IntMap v -> Event t (PatchIntMap v) -> DomBuilderEff t m r (IntMap v', Event t (PatchIntMap v'))
  TraverseDMapWithKeyWithAdjustWithMoveE :: (forall a. k a -> v a -> r (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> DomBuilderEff t m r (DMap k v', Event t (PatchDMapWithMove k v'))

instance (Member (DomBuilderEff t m) r, Adjustable t m) => Adjustable t (SemR t m r) where
  runWithReplace a eb = SemR $ send @(DomBuilderEff t m) (RunWithReplaceE (unSemR a) (fmapCheap unSemR eb))
  traverseIntMapWithKeyWithAdjust f i e = SemR $ send @(DomBuilderEff t m) (TraverseIntMapWithKeyWithAdjustE (\k v -> unSemR (f k v)) i e)
  traverseDMapWithKeyWithAdjustWithMove f i e = SemR $ send @(DomBuilderEff t m) (TraverseDMapWithKeyWithAdjustWithMoveE (\k v -> unSemR (f k v)) i e)

instance (Member (Lift m) r, Member (DomBuilderEff t m) r, DomBuilder t m) => DomBuilder t (SemR t m r) where
  type DomBuilderSpace (SemR t m r) = DomBuilderSpace m
  inputElement cfg = SemR $ sendM @m (inputElement cfg)
  textAreaElement cfg = SemR $ sendM @m (textAreaElement cfg)
  textNode cfg = SemR $ sendM @m (textNode cfg)
  commentNode cfg = SemR $ sendM @m (commentNode cfg)
  placeRawElement e = SemR $ sendM @m (placeRawElement e)
  wrapRawElement e cfg = SemR $ sendM @m (wrapRawElement e cfg)
  selectElement cfg inner = SemR $ send @(DomBuilderEff t m) (SelectElementE cfg (unSemR inner))
  element e cfg child = SemR $ send @(DomBuilderEff t m) (ElementE e cfg (unSemR child))

runDomBuilder ::
     forall t m r a. (DomBuilder t m, Member Fixpoint r, MonadHold t m, Member (Lift m) r)
  => (forall x. Semantic r x -> m x)
  -> Semantic (DomBuilderEff t m ': r) a
  -> Semantic r a
runDomBuilder runner' = interpretH $ \case
  ElementE e cfg child -> do
    c <- runT child
    sendM $ do
      (out, fa) <- element e cfg (runner c)
      pure $ (out,) <$> fa
  RunWithReplaceE (a :: m1 a1) (eb :: Event t (m1 b)) -> do
    ra :: m (f a1) <- runner <$> runT a
    rec (resA :: f a1, resB) <- sendM @m $ runWithReplace ra (reb :: Event t (m (f (a1, b))))
        run <- bindT runner
        ds <- sendM @m $ holdDyn ((, undefined) <$> resA) resB
        let reb = flip pushAlways eb $ \(b :: m1 b1) -> do
              s :: f (a1, b) <- sample (current ds)
              pure _ -- . sequence $ ffor b (\b' -> traverse runner $ ffor s $ \(a, _) -> (a, b'))
    pure $ (\(a, b) -> (a, updated b)) . sequence <$> sequence ds

  SelectElementE cfg child -> do
    c <- runT child
    sendM $ do
      (sel, fa) <- selectElement cfg (runner c)
      pure $ (sel,) <$> fa

  where
    runner :: Semantic (DomBuilderEff t m : r) z -> m z
    runner = runner' .@ runDomBuilder

main :: IO ()
main = JS.run 3000 $ mainWidget go
  where
    go :: forall t m. (Typeable t, MonadWidget t m) => m ()
    go = (runM .@ runFixpointM .@ runDomBuilder) . runStore @Text @t @m "" $ unSemR app

app :: forall t m. (DomBuilder t m, PostBuild t m) => SemR t m '[Store Text t, DomBuilderEff t m, Fixpoint, Lift m] ()
app = do
  x <- SemR $ get @Text @t
  dynText x
  dText <- inputElement def
  eSave <- button "Save"
  SemR $ put (current (value dText) <@ eSave)
