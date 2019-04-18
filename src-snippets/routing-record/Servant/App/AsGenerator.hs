{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.App.AsGenerator
  ( runGen
  , gen
  , HasGen(..)
  ) where

import Control.Monad (void)
import Control.Monad.Reader (Reader, MonadReader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, execWriterT, tell)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)), cons, uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex.Dom.Core (StaticWidget, renderStatic, el)
import Servant.App.AsApp (AsApp, HasApp(..))
import Servant.App.Types (App, Loc(..))
import Servant.API ((:<|>)(..), (:>), Capture, IsElem, QueryParam, QueryParams, ToHttpApiData(..))
import Servant.API.Generic (AsApi, ToServantApi)
import System.Directory (createDirectoryIfMissing)

import Unsafe.Coerce (unsafeCoerce)


-- type family Flatten (api :: k) :: k where
--   Flatten ((a :: k) :> (b :<|> c)) = (a :> Flatten b) :<|> (a :> Flatten c)
--   Flatten ((a :: k) :> b)          = Redex b (Flatten b) a
--   Flatten (a :<|> b)               = Flatten a :<|> Flatten b
--   Flatten (a :: k)                 = a

-- type family Redex a b (c :: k) :: * where
--   Redex a a first = Flatten first :> a
--   Redex a b first = Flatten (first :> b)

runGen ::
     Monad m
  => Text
  -> (m () -> StaticWidget x a)
  -> StaticWidget x ()
  -> app
  -> GenM app m ()
  -> IO ()
runGen start nt hd app f = runRenderTree start nt hd (toRenderTree $ runGenM app f)

data RenderTree m = RenderTree
  { rtValue :: Maybe (m ())
  , rtChildren :: Map Text (RenderTree m)
  }

runRenderTree ::
     forall m x a.
     Text
  -> (m () -> StaticWidget x a)
  -> StaticWidget x ()
  -> Map Text (RenderTree m)
  -> IO ()
runRenderTree start nt hd = void . M.traverseWithKey (go start)
  where
    go :: Text -> Text -> RenderTree m -> IO ()
    go ps p rt = do
      for_ (rtValue rt) $ \app -> do
        bs <- runRenderAction nt hd app
        createDirectoryIfMissing True (T.unpack ps)
        BS.writeFile (T.unpack $ ps <> "/" <> p ) bs
      void $ M.traverseWithKey (go $ ps <> "/" <> p) (rtChildren rt)

toRenderTree :: [RenderAction m] -> Map Text (RenderTree m)
toRenderTree = foldr go M.empty . fmap (\ra -> (htmlify . locPath $ renderLoc ra, renderApp ra))
  where
    go :: (NonEmpty Text, m ()) -> Map Text (RenderTree m) -> Map Text (RenderTree m)
    go (loc, app) = case uncons loc of
      (p, Nothing) -> flip M.alter p $ \case
        Nothing -> Just $ RenderTree (Just app) M.empty
        Just rt -> Just $ rt { rtValue = Just app }
      (p, Just ps) -> flip M.alter p $ \case
        Nothing -> Just $ RenderTree Nothing (go (ps, app) M.empty)
        Just rt -> Just $ rt { rtChildren = go (ps, app) (rtChildren rt) }

runRenderAction ::
     (m () -> StaticWidget x a)
  -> StaticWidget x ()
  -> m ()
  -> IO ByteString
runRenderAction nt hd app = snd <$> renderStatic (topLevel hd . (() <$) $ nt app)

topLevel :: StaticWidget x () -> StaticWidget x () -> StaticWidget x ()
topLevel hd body =
  el "html" $ do
    el "head" hd
    el "body" body

htmlify :: [Text] -> NonEmpty Text
htmlify = \case
  [] -> "index.html" :| []
  [""] -> "index.html" :| []
  [x] -> (x <> ".html") :| []
  x:xs -> cons x (htmlify xs)


newtype GenM app m a = GenM
  { unGenM :: WriterT [RenderAction m] (Reader app) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader app
             , MonadWriter [RenderAction m]
             )

runGenM :: app -> GenM app m () -> [RenderAction m]
runGenM app f = runReader (execWriterT (unGenM f)) app

gen ::
     forall top m e.
     ( IsElem e (ToServantApi top)
     , HasGen e
     , Monad m
     )
  => (top AsApi -> e)
  -> MkRender e m (GenM (top (AsApp m)) m)
gen f = genR (Proxy @m) (Proxy @e) id
  (asks (unsafeCoerce f) :: GenM (top (AsApp m)) m (MkApp e m))


data RenderAction m = RenderAction
  { renderLoc :: Loc
  , renderApp :: m ()
  }

class HasGen api where
  type MkRender api (m :: * -> *) (m0 :: * -> *) :: *
  genR ::
       MonadWriter [RenderAction m] m0
    => Proxy m
    -> Proxy api
    -> (Loc -> Loc)
    -> m0 (MkApp api m)
    -> MkRender api m m0

instance (HasGen a, HasGen b) => HasGen (a :<|> b) where
  type MkRender (a :<|> b) m m0 = MkRender a m m0 :<|> MkRender b m m0
  genR m _ lf s =
    genR m (Proxy @a) lf ((\(a :<|> _) -> a) <$> s) :<|>
    genR m (Proxy @b) lf ((\(_ :<|> b) -> b) <$> s)

instance (KnownSymbol sym, HasGen sub) => HasGen (sym :> sub) where
  type MkRender (sym :> sub) m m0 = MkRender sub m m0
  genR m _ lf = genR m (Proxy @sub) $
    lf . (\l -> l { locPath = toUrlPiece (symbolVal $ Proxy @sym) : locPath l })

instance (ToHttpApiData a, HasGen sub) => HasGen (Capture sym a :> sub) where
  type MkRender (Capture sym a :> sub) m m0 = a -> MkRender sub m m0
  genR m _ lf f a = genR m (Proxy @sub)
    (lf . (\l -> l { locPath = toUrlPiece a : locPath l })) (($ a) <$> f)

instance (ToHttpApiData a, KnownSymbol sym, HasGen sub) => HasGen (QueryParam sym a :> sub) where
  type MkRender (QueryParam sym a :> sub) m m0 = Maybe a -> MkRender sub m m0
  genR m _ lf f ma = genR m (Proxy @sub)
    (lf . (\l -> maybe l (\a -> l { locQuery = (sym, toQueryParam a) : locQuery l}) ma))
    (($ ma) <$> f)
    where
      sym = T.pack . symbolVal $ Proxy @sym

instance (ToHttpApiData a, KnownSymbol sym, HasGen sub) => HasGen (QueryParams sym a :> sub) where
  type MkRender (QueryParams sym a :> sub) m m0 = [a] -> MkRender sub m m0
  genR m _ lf f ma = genR m (Proxy @sub)
    (lf . (\l -> foldr (\a l' -> l' { locQuery = (sym, toQueryParam a) : locQuery l'}) l ma))
    (($ ma) <$> f)
    where
      sym = T.pack . symbolVal $ Proxy @sym

instance HasGen App where
  type MkRender App m m0 = m0 ()
  genR _ _ l f = do
    app <- f
    tell [RenderAction (l $ Loc [] []) app]
