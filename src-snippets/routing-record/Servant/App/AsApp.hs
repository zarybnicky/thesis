{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.App.AsApp
  ( AsApp
  , HasApp(..)
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (find, partition)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex.Dom.Core ()
import Servant.App.Types (App, Err(..), Loc(..))
import Servant.API ((:<|>)(..), (:>), Capture, QueryParam, QueryParams)
import Servant.API.Generic (GenericMode(..))

data AsApp (t :: *) (m :: * -> *)

instance GenericMode (AsApp t m) where
  type AsApp t m :- api = AppT api t m

class HasApp api ctx where
  type AppT api t (m :: * -> *) :: *
  route :: Proxy api -> context ctx t -> AppT api t m -> Loc -> Either Err (m ())

instance (HasApp a ctx, HasApp b ctx) => HasApp (a :<|> b) ctx where
  type AppT (a :<|> b) t m = AppT a t m :<|> AppT b t m
  route _ ctx (a :<|> b) = route (Proxy @a) ctx a <> route (Proxy @b) ctx b

instance (KnownSymbol sym, HasApp sub ctx) => HasApp (sym :> sub) ctx where
  type AppT (sym :> sub) t m = AppT sub t m
  route _ ctx f loc = case locPath loc of
    [] -> Left Err404
    p:ps ->
      if p == BC.pack (symbolVal $ Proxy @sym)
      then route (Proxy @sub) ctx f (loc { locPath = ps })
      else Left Err404

instance (FromJSON a, KnownSymbol sym, HasApp sub ctx) => HasApp (Capture sym a :> sub) ctx where
  type AppT (Capture sym a :> sub) t m = a -> AppT sub t m
  route _ ctx f loc = case locPath loc of
    [] -> Left Err404
    x:xs -> case eitherDecode x of
      Right p -> route (Proxy @sub) ctx (f p) (loc { locPath = xs })
      Left _ ->
        let s = T.pack $ symbolVal (Proxy @sym)
            v = T.pack $ BC.unpack x
        in Left $ Err400 ("Error parsing path part '" <> v <> "' ('" <> s <> "'")

instance (FromJSON a, KnownSymbol sym, HasApp sub ctx) => HasApp (QueryParam sym a :> sub) ctx where
  type AppT (QueryParam sym a :> sub) t m = Maybe a -> AppT sub t m
  route _ ctx f loc = case find ((s ==) . fst) (locQuery loc) of
    Nothing -> route (Proxy @sub) ctx (f Nothing) loc
    Just (_, x) -> case eitherDecode x of
      Right p -> route (Proxy @sub) ctx (f (Just p)) loc
      Left e ->
        let param = T.pack $ symbolVal (Proxy @sym)
        in Left $ Err400 ("Error parsing parameter " <> param <> ": " <> T.pack e)
    where
      s = BC.pack $ symbolVal (Proxy @sym)

instance (FromJSON a, KnownSymbol sym, HasApp sub ctx) => HasApp (QueryParams sym a :> sub) ctx where
  type AppT (QueryParams sym a :> sub) t m = [a] -> AppT sub t m
  route _ ctx f loc = case partition ((s ==) . fst) (locQuery loc) of
    (xs, _) -> case partitionEithers (eitherDecode . snd <$> xs) of
      ([], ps) -> route (Proxy @sub) ctx (f ps) loc
      (errs, _) ->
        let param = T.pack $ symbolVal (Proxy @sym)
            errs' = T.intercalate ", " (T.pack <$> errs)
        in Left $ Err400 ("Error parsing parameter " <> param <> ": " <> errs')
    where
      s = BC.pack $ symbolVal (Proxy @sym)

instance HasApp App ctx where
  type AppT App t m = m ()
  route _ _ f loc = case locPath loc of
    [] -> Right f
    [""] -> Right f
    _ -> Left Err404

-- data Authorized
-- instance (HasContextElement AuthCheck ctx, HasApp sub ctx) => HasApp (AuthProtect Authorized :> sub) ctx where
--   type AppT (AuthProtect Authorized :> sub) t m = AppT sub t m
--   route _ ctx f = route (Proxy @sub) ctx f
