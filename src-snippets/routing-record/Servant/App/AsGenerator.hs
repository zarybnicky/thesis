{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.App.AsGenerator
  ( runGen
  , gen
  , HasGen(..)
  ) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Servant.App
import Servant.API
import Servant.API.Generic

import Unsafe.Coerce (unsafeCoerce)


-- type family Flatten (api :: k) :: k where
--   Flatten ((a :: k) :> (b :<|> c)) = (a :> Flatten b) :<|> (a :> Flatten c)
--   Flatten ((a :: k) :> b)          = Redex b (Flatten b) a
--   Flatten (a :<|> b)               = Flatten a :<|> Flatten b
--   Flatten (a :: k)                 = a

-- type family Redex a b (c :: k) :: * where
--   Redex a a first = Flatten first :> a
--   Redex a b first = Flatten (first :> b)

newtype GenM app m a = GenM
  { runGenM :: WriterT [(Loc, m ())] (Reader app) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader app
             , MonadWriter [(Loc, m ())]
             )

runGen :: Monad m => app -> GenM app m () -> [(Loc, m ())]
runGen app f = runReader (execWriterT (runGenM f)) app

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


class HasGen api where
  type MkRender api (m :: * -> *) (m0 :: * -> *) :: *
  genR ::
       MonadWriter [(Loc, m ())] m0
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
  genR _ _ l f = tell =<< (:[]) . (l $ Loc [] [],) <$> f
