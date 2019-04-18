{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.App.AsAppLink
  ( HasAppLink(..)
  , GetSkipped
  , appLink
  , safeAppLink
  , (.>)
  , (.>!)
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex.Dom.Core (Event, EventWriter, Reflex, tellEvent)
import Servant.App.TupleProduct (TupleProduct(..), unconsT)
import Servant.App.Types (App, Loc(..))
import Servant.API ((:>), Capture, IsElem, QueryParam, QueryParams, ToHttpApiData(..))
import Servant.API.Generic
  ( AsApi
  , GServantProduct
  , GenericMode
  , GenericServant
  , ToServant
  , ToServantApi
  , genericApi
  , fromServant
  )
import Servant.API.TypeLevel (IsSubAPI)

appLink ::
     forall t e rs m.
     ( EventWriter t Loc m
     , Reflex t
     , IsElem e (ToServantApi rs)
     , GenericServant rs AsApi
     , HasAppLink e
     )
  => (rs AsApi -> e)
  -> Event t (TupleProductOf (GatherLinkArgs e))
  -> m ()
appLink _ args =
  tellEvent $
  safeAppLink (genericApi (Proxy :: Proxy rs)) (Proxy @e) (Loc [] []) <$> args

safeAppLink ::
     (IsElem e api, HasAppLink e)
  => Proxy api
  -> Proxy e
  -> Loc
  -> TupleProductOf (GatherLinkArgs e)
  -> Loc
safeAppLink _ = toAppLink

(.>) ::
     forall sup sub mode e.
     ( GenericMode mode
     , Generic (sub mode)
     , GServantProduct (Rep (sub mode))
     )
  => (sup mode -> ToServant sub mode)
  -> (sub mode -> e)
  -> (sup mode -> e)
sup .> sub = \api -> sub (fromServant $ sup api :: sub mode)

(.>!) ::
     forall sub eSub top e result.
     ( IsSubAPI result (ToServantApi top)
     , Generic (sub AsApi)
     , GServantProduct (Rep (sub AsApi))
     , result ~ GetSkipped e eSub
     )
  => (top AsApi -> eSub)
  -> (sub AsApi -> e)
  -> (top AsApi -> result)
_ .>! _ = undefined

type family GetSkipped sub api where
  GetSkipped sub sub = sub
  GetSkipped sub (top :> notApi) = top :> GetSkipped sub notApi
--  GetSkipped sub (a :<|> b) = OrSkip sub (GetSkipped sub a) (GetSkipped sub b)


class HasAppLink api where
  type GatherLinkArgs api :: [*]
  toAppLink :: Proxy api -> Loc -> TupleProductOf (GatherLinkArgs api) -> Loc

-- All links
-- instance (HasAppLink a, HasAppLink b) => HasAppLink (a :<|> b) where
--   type GatherLinkArgs (a :<|> b) = GatherLinkArgs a :<|> GatherLinkArgs b
--   toAppLink _ p q r f = toAppLink (Proxy @a) p q r f :<|> toAppLink (Proxy @b) p q r f

instance (KnownSymbol sym, HasAppLink sub) => HasAppLink (sym :> sub) where
  type GatherLinkArgs (sym :> sub) = GatherLinkArgs sub
  toAppLink _ l = toAppLink (Proxy @sub) $ l
    { locPath = locPath l ++ [toUrlPiece . symbolVal $ Proxy @sym]
    }

instance (ToHttpApiData a, HasAppLink sub, TupleProduct (GatherLinkArgs sub)) =>
         HasAppLink (Capture sym a :> sub) where
  type GatherLinkArgs (Capture sym a :> sub) = a : GatherLinkArgs sub
  toAppLink _ l args =
    case unconsT (Proxy @(a : GatherLinkArgs sub)) args of
      (r, rs) ->
        toAppLink (Proxy @sub) (l {locPath = locPath l ++ [toUrlPiece r]}) rs

instance (ToHttpApiData a, HasAppLink sub, KnownSymbol sym, TupleProduct (GatherLinkArgs sub)) =>
         HasAppLink (QueryParam sym a :> sub) where
  type GatherLinkArgs (QueryParam sym a :> sub) = Maybe a : GatherLinkArgs sub
  toAppLink _ l args =
    case unconsT (Proxy @(Maybe a : GatherLinkArgs sub)) args of
      (Nothing, rs) -> toAppLink (Proxy @sub) l rs
      (Just x, rs) ->
        toAppLink (Proxy @sub) (l {locQuery = (sym, toQueryParam x):locQuery l}) rs
    where
      sym = T.pack . symbolVal $ Proxy @sym

instance (ToHttpApiData a, HasAppLink sub, KnownSymbol sym, TupleProduct (GatherLinkArgs sub)) =>
         HasAppLink (QueryParams sym a :> sub) where
  type GatherLinkArgs (QueryParams sym a :> sub) = [a] : GatherLinkArgs sub
  toAppLink _ l args =
    case unconsT (Proxy @([a] : GatherLinkArgs sub)) args of
      (r, rs) ->
        toAppLink (Proxy @sub) (l {locQuery = ((sym,) . toQueryParam <$> r) ++ locQuery l}) rs
    where
      sym = T.pack . symbolVal $ Proxy @sym

instance HasAppLink App where
  type GatherLinkArgs App = '[]
  toAppLink _ l _ = l
