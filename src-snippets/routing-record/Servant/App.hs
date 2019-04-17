{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.App
  ( Loc(..)
  , Err(..)
  , App
  , AsApp
  , HasApp(..)
  , HasAppLink(..)
  , serve
  , url
  , appLink
  , (.^)
  , TupleProduct(..)
  , GenericMode(..)
  , genericApi
  , ToServantApi
  , toServant
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Bifunctor (bimap, first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (find, partition)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.History (pushState)
import GHCJS.DOM.Location (getHref)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getHistory, getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import Reflex.Dom.Core
import Servant.API
  ( (:<|>)(..)
  , (:>)
  , Capture
  , IsElem
  , QueryParam
  , QueryParams
  )
import Servant.API.Generic
  ( AsApi
  , GenericMode(..)
  , GenericServant
  , GServantProduct
  , ToServantApi
  , genericApi
  , toServant
  )
import Servant.API.TypeLevel (IsSubAPI)
import URI.ByteString
  (Query(..), URI, URIParseError, laxURIParserOptions, parseURI,
   serializeURIRef', uriFragment, uriPath, uriQuery)


data Loc = Loc
  { locPath :: [ByteString]
  , locQuery :: [(ByteString, ByteString)]
  }
instance Semigroup Loc where
  a <> _ = a

data Err
  = Err400 Text
  | Err401
  | Err404
  | Err500 URIParseError
  | Err501
  deriving (Eq, Show)

url ::
     ( MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , DomBuilder t m
     , MonadJSM (Performable m)
     , MonadJSM m
     )
  => Event t Loc
  -> m (Dynamic t (Either Err Loc))
url us = do
  window <- currentWindowUnchecked
  location <- getLocation window
  history <- getHistory window
  text2uri <$> getHref location >>= \case
    Left e -> do
      text ("Failed to parse URI, cannot continue: " <> T.pack (show e))
      pure . constDyn $ Left e
    Right u0 -> do
      performEvent_ $ pushState history () ("" :: Text) . Just . uri2text . loc2uri u0 <$> us
      ps <- wrapDomEvent window (`on` popState) $ fmap uri2loc . text2uri <$> getHref location
      holdDyn (Right $ uri2loc u0) (leftmost [ps, Right <$> us])
  where
    text2uri = first Err500 . parseURI laxURIParserOptions . BC.toStrict . BC.pack . T.unpack
    uri2text = T.pack . BC.unpack . BC.fromStrict . serializeURIRef'
    uri2loc uri = Loc
      { locPath = drop 1 . BC.split '/' . BC.fromStrict $ uriPath uri
      , locQuery = bimap BC.fromStrict BC.fromStrict <$> queryPairs (uriQuery uri)
      }
    loc2uri :: URI -> Loc -> URI
    loc2uri u0 loc = u0
      { uriPath = "/" <> BC.toStrict (BC.intercalate "/" (locPath loc))
      , uriQuery = Query $ bimap BC.toStrict BC.toStrict <$> locQuery loc
      , uriFragment = Nothing
      }


-- error page :: (api -> AppT) -> Err -> m ()
-- Think about /error?redirect=/admin/x

serve ::
     forall t m api. (HasApp api (), MonadWidget t m)
  => Proxy api
  -> AppT api t (EventWriterT t Loc m)
  -> (Err -> EventWriterT t Loc m ())
  -> m ()
serve api ws showError = mdo
  let ctx = undefined :: Context () t
  dUrl <- url eUrl
  (_, eUrl) <- runEventWriterT $ dyn $ either showError id . (route api ctx ws =<<) <$> dUrl
  pure ()

data Context ctx t

-- data Context contextTypes where
--     EmptyContext :: Context '[]
--     (:.) :: x -> Context xs -> Context (x ': xs)
-- infixr 5 :.

-- class HasContextEntry (context :: [*]) (val :: *) where
--   getContextEntry :: Context context -> val

-- instance {-# OVERLAPPABLE #-} HasContextEntry xs val => HasContextEntry (notIt ': xs) val where
--   getContextEntry (_ :. xs) = getContextEntry xs

-- instance {-# OVERLAPPING #-} HasContextEntry (val ': xs) val where
--   getContextEntry (x :. _) = x

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

data App
instance HasApp App ctx where
  type AppT App t m = m ()
  route _ _ f loc = case locPath loc of
    [] -> Right f
    [""] -> Right f
    _ -> Left Err404

-- AuthProtect tag :>
-- data Authorized
-- instance (HasContextElement AuthCheck ctx, HasApp sub ctx) => HasApp (AuthProtect Authorized :> sub) ctx where
--   type AppT (AuthProtect Authorized :> sub) t m = AppT sub t m
--   route _ ctx f = route (Proxy @sub) ctx f

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

appLink ::
     forall t m e rs.
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


-- TODO: Figure out a real proof...
(.^) :: forall sub eSub top e result.
     ( IsSubAPI result (ToServantApi top)
     , Generic (sub AsApi)
     , GServantProduct (Rep (sub AsApi))
     , result ~ GetSkipped (ToServantApi sub) eSub
     )
  => (top AsApi -> eSub)
  -> (sub AsApi -> e)
  -> (top AsApi -> result)
_ .^ _ = undefined

type family GetSkipped sub api where
  GetSkipped sub (top :> notApi) = top :> GetSkipped sub notApi
  GetSkipped sub s = s

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
    { locPath = locPath l ++ [BC.pack . symbolVal $ Proxy @sym]
    }

instance ( ToJSON a
         , HasAppLink sub
         , TupleProduct (GatherLinkArgs sub)
         ) =>
         HasAppLink (Capture sym a :> sub) where
  type GatherLinkArgs (Capture sym a :> sub) = a : GatherLinkArgs sub
  toAppLink _ l (unconsT (Proxy @(a : GatherLinkArgs sub)) -> (r, rs)) =
    toAppLink (Proxy @sub) (l {locPath = locPath l ++ [encode r]}) rs

instance HasAppLink App where
  type GatherLinkArgs App = '[]
  toAppLink _ l _ = l
