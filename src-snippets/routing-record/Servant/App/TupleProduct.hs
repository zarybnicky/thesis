-- Source:
-- https://github.com/Compositional/reflex-servant/blob/master/src/Reflex/Servant/Internal/Product.hs

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.App.TupleProduct
  ( unconsT
  , TupleProduct(..)
  ) where

import Data.Proxy (Proxy(..))

unconsT ::
     forall a hd tl. (a ~ (hd : tl), TupleProduct tl)
  => Proxy a
  -> TupleProductOf a
  -> (hd, TupleProductOf tl)
unconsT _ a = (tupleHead (Proxy @tl) a, tupleTail (Proxy @tl) (Proxy @hd) a)

class TupleProduct ts where
  type TupleProductOf (ts :: [*]) :: *
  tupleCons :: Proxy ts -> hd -> TupleProductOf ts -> TupleProductOf (hd ': ts)
  tupleHead :: Proxy ts -> TupleProductOf (hd ': ts) -> hd
  tupleTail :: Proxy ts -> Proxy hd -> TupleProductOf (hd ': ts) -> TupleProductOf ts

instance TupleProduct '[] where
  type TupleProductOf '[] = ()
  tupleCons _ hd _ = hd
  tupleHead _ hd = hd
  tupleTail _ _ _ = ()

instance TupleProduct '[a0] where
  type TupleProductOf '[a0] = a0
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, a) = a

instance TupleProduct '[a1, a0] where
  type TupleProductOf '[a1, a0] = (a1, a0)
  tupleCons _ hd (a1, a0) = (hd, a1, a0)
  tupleHead _ (a, _, _) = a
  tupleTail _ _ (_, a1, a0) = (a1, a0)

instance TupleProduct '[a2, a1, a0] where
  type TupleProductOf '[a2, a1, a0] = (a2, a1, a0)
  tupleCons _ hd (a2, a1, a0) = (hd, a2, a1, a0)
  tupleHead _ (a, _, _, _) = a
  tupleTail _ _ (_, a2, a1, a0) = (a2, a1, a0)

instance TupleProduct '[a3, a2, a1, a0] where
  type TupleProductOf '[a3, a2, a1, a0] = (a3, a2, a1, a0)
  tupleCons _ hd (a3, a2, a1, a0) = (hd, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _) = a
  tupleTail _ _ (_, a3, a2, a1, a0) = (a3, a2, a1, a0)

instance TupleProduct '[a4, a3, a2, a1, a0] where
  type TupleProductOf '[a4, a3, a2, a1, a0] = (a4, a3, a2, a1, a0)
  tupleCons _ hd (a4, a3, a2, a1, a0) = (hd, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _) = a
  tupleTail _ _ (_, a4, a3, a2, a1, a0) = (a4, a3, a2, a1, a0)

instance TupleProduct '[a5, a4, a3, a2, a1, a0] where
  type TupleProductOf '[a5, a4, a3, a2, a1, a0] = (a5, a4, a3, a2, a1, a0)
  tupleCons _ hd (a5, a4, a3, a2, a1, a0) = (hd, a5, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _, _) = a
  tupleTail _ _ (_, a5, a4, a3, a2, a1, a0) = (a5, a4, a3, a2, a1, a0)

instance TupleProduct '[a6, a5, a4, a3, a2, a1, a0] where
  type TupleProductOf '[a6, a5, a4, a3, a2, a1, a0] = (a6, a5, a4, a3, a2, a1, a0)
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl

instance TupleProduct (a7 ': a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more) where
  type TupleProductOf (a7 ': a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more) =
        (a7, TupleProductOf (a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more))
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl
