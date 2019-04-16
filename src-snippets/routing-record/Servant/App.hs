

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.App
  ( app
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Either (partitionEithers)
import Data.List (find, partition)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.History (pushState)
import GHCJS.DOM.Location (getHref)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.Window (getHistory, getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import Network.HTTP.Media ((//), (/:))
import Reflex.Dom.Core
import Servant.API hiding (URI(..))
import Servant.API.Generic
import Servant.API.TypeLevel
import URI.ByteString


url ::
     ( MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , MonadJSM m
     )
  => Event t Loc
  -> m (Dynamic t (Either URIParseError Loc))
url us = do
  window <- currentWindowUnchecked
  location <- getLocation window
  history <- getHistory window
  loc0 <- text2uri <$> getHref location
  let u0 = either (error . ("Failed to parse URI: " <>) . show) id loc0
  performEvent_ $ pushState history () ("" :: Text) . Just . uri2text . loc2uri u0 <$> us
  ps <- wrapDomEvent window (`on` popState) $ fmap uri2loc . text2uri <$> getHref location
  holdDyn (Right $ uri2loc u0) (leftmost [ps, Right <$> us])
  where
    text2uri = parseURI laxURIParserOptions . BC.toStrict . BC.pack . T.unpack
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


app :: MonadWidget t m => m ()
app = serve (Proxy @(ToServantApi Routes)) (toServant widgets)

widgets :: forall t m. MonadWidget t m => Routes (AsApp t (EventWriterT t Loc m))
widgets = Routes
  { showUserRoute = \(i :: Int) -> text (T.pack $ show i)
  , homeRoute = do
      e <- button "Go to user"
      tellEvent $ Loc ["user", "55"] [] <$ e
  }

data Routes route = Routes
  { showUserRoute :: route :- "user" :> Capture "id" Int :> Get '[App] ()
  , homeRoute :: route :- Get '[App] ()
  } deriving (Generic)

data Context ctx t

-- Think about /error?redirect=/admin/x
serve ::
     forall t m api. (HasApp api (), MonadWidget t m)
  => Proxy api
  -> AppT api t (EventWriterT t Loc m)
  -> m ()
serve api widgets = mdo
  let showError :: Show a => a -> EventWriterT t Loc m ()
      showError = text . T.pack . show
  dUrl <- url eUrl
  let ctx = undefined :: Context () t
  let app = either showError (either showError id . route api ctx widgets) <$> dUrl
  (_, eUrl) <- runEventWriterT $ dyn app
  pure ()

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
  | Err501
  deriving (Eq, Ord, Show)

data App

instance Accept App where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- Render a hydrated app on the server
-- instance MimeRender App a where
--   mimeRender _ _ = ...

data AsApp (t :: *) (m :: * -> *)

instance GenericMode (AsApp t m) where
  type AsApp t m :- api = AppT api t m

class HasApp api ctx where
  type AppT api t (m :: * -> *) :: *
  route :: Proxy api -> context ctx t -> AppT api t m -> Loc -> Either Err (m ())

-- :<|>
instance (HasApp a ctx, HasApp b ctx) => HasApp (a :<|> b) ctx where
  type AppT (a :<|> b) t m = AppT a t m :<|> AppT b t m
  route _ ctx (a :<|> b) = route (Proxy @a) ctx a <> route (Proxy @b) ctx b

-- Symbol :>
instance (KnownSymbol sym, HasApp sub ctx) => HasApp (sym :> sub) ctx where
  type AppT (sym :> sub) t m = AppT sub t m
  route _ ctx f loc = case locPath loc of
    [] -> Left Err404
    p:ps ->
      if p == BC.pack (symbolVal $ Proxy @sym)
      then route (Proxy @sub) ctx f (loc { locPath = ps })
      else Left Err404

-- Capture sym a :>
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

-- QueryParam sym a :>
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

-- QueryParams sym a :>
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

-- AuthProtect tag :>
-- data Authorized
-- instance (HasContextElement AuthCheck ctx, HasApp sub ctx) => HasApp (AuthProtect Authorized :> sub) ctx where
--   type AppT (AuthProtect Authorized :> sub) t m = AppT sub t m
--   route _ ctx f = route (Proxy @sub) ctx f

-- Verb - IsElem App contents, method ~ Get?
instance (Elem App ct) => HasApp (Verb 'GET st ct a) ctx where
  type AppT (Verb 'GET st ct a) t m = m ()
  route _ _ f loc = case locPath loc of
    [] -> Right f
    [""] -> Right f
    _ -> Left Err404
