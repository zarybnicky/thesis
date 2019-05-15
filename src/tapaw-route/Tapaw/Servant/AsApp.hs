{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tapaw.Servant.AsApp
  ( AsApp
  , HasApp(..)
  ) where

import Data.Either (partitionEithers)
import Data.List (find, partition)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex.Dom.Core ()
import Tapaw.Servant.Types (App, Err(..), Loc(..))
import Servant.API ((:<|>)(..), (:>), Capture, FromHttpApiData(..), QueryParam, QueryParams)
import Servant.API.Generic (GenericMode(..))


data AsApp m

instance GenericMode (AsApp m) where
  type AsApp m :- api = MkApp api m

class HasApp api where
  type MkApp api m :: *
  route :: Proxy api -> MkApp api m -> Loc -> Either Err m

instance (HasApp a, HasApp b) => HasApp (a :<|> b) where
  type MkApp (a :<|> b) m = MkApp a m :<|> MkApp b m
  route _ (a :<|> b) = route (Proxy @a) a <> route (Proxy @b) b

instance (KnownSymbol sym, HasApp sub) => HasApp (sym :> sub) where
  type MkApp (sym :> sub) m = MkApp sub m
  route _ f loc = case locPath loc of
    [] -> Left Err404
    p:ps ->
      if p == T.pack (symbolVal $ Proxy @sym)
      then route (Proxy @sub) f (loc { locPath = ps })
      else Left Err404

instance (FromHttpApiData a, KnownSymbol sym, HasApp sub) => HasApp (Capture sym a :> sub) where
  type MkApp (Capture sym a :> sub) m = a -> MkApp sub m
  route _ f loc = case locPath loc of
    [] -> Left Err404
    x:xs -> case parseUrlPiece x of
      Right p -> route (Proxy @sub) (f p) (loc { locPath = xs })
      Left _ ->
        let s = T.pack $ symbolVal (Proxy @sym)
        in Left $ Err400 ("Error parsing path part '" <> x <> "' ('" <> s <> "'")

instance (FromHttpApiData a, KnownSymbol sym, HasApp sub) => HasApp (QueryParam sym a :> sub) where
  type MkApp (QueryParam sym a :> sub) m = Maybe a -> MkApp sub m
  route _ f loc = case find ((s ==) . fst) (locQuery loc) of
    Nothing -> route (Proxy @sub) (f Nothing) loc
    Just (_, x) -> case parseQueryParam x of
      Right p -> route (Proxy @sub) (f (Just p)) loc
      Left e ->
        let param = T.pack $ symbolVal (Proxy @sym)
        in Left $ Err400 ("Error parsing parameter " <> param <> ": " <> e)
    where
      s = T.pack $ symbolVal (Proxy @sym)

instance (FromHttpApiData a, KnownSymbol sym, HasApp sub) => HasApp (QueryParams sym a :> sub) where
  type MkApp (QueryParams sym a :> sub) m = [a] -> MkApp sub m
  route _ f loc = case partition ((s ==) . fst) (locQuery loc) of
    (xs, _) -> case partitionEithers (parseQueryParam . snd <$> xs) of
      ([], ps) -> route (Proxy @sub) (f ps) loc
      (errs, _) ->
        let param = T.pack $ symbolVal (Proxy @sym)
            errs' = T.intercalate ", " errs
        in Left $ Err400 ("Error parsing parameter " <> param <> ": " <> errs')
    where
      s = T.pack $ symbolVal (Proxy @sym)

instance HasApp App where
  type MkApp App m = m
  route _ f loc = case locPath loc of
    [] -> Right f
    [""] -> Right f
    _ -> Left Err404
