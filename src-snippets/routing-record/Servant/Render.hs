{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Render
  ( module Link
  , ServantErr(..)
  , Env(..)
  , Scheme(..)
  , Authority(..)
  , Uri(..)
  , Reflexive(..)
  , Render(..)
  , HasRender(..)
  , SupportsServantRender
  , Runtime
  , AsRender
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Reflex.Class (MonadSample(..), Reflex(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as LB
import Data.Semigroup ((<>))
import Data.Bifunctor (first)
import Network.HTTP.Media ((//), (/:), renderHeader)
import Servant.API hiding (Link)
import Servant.API.Generic
import Servant.Common.Uri
  ( Authority(..)
  , QueryPiece(..)
  , Scheme(..)
  , Uri(..)
  , unconsPathPiece
  , unconsQuery
  )
import Servant.Common.Req
  ( Req(..)
  , SupportsServantRender
  , addBody
  , performOneRequest
  , performRequestCT
  , performRequestCT'
  , prependHeader
  , prependPathPiece
  , prependQueryParam
  )
import Reflex.Link as Link


newtype Reflexive a =
  Reflexive a
  deriving (Read, Show, Eq, Ord)

data ServantErr
  = NotFound T.Text
  | AjaxFailure T.Text

data Env t m = Env
  { envAuthority :: Dynamic t Authority
  , envOnFailure :: ServantErr -> Link t m
  , envFailUri :: Uri
  }

data Render api t m = Render
  { renderRouter :: Uri -> Uri -> Either ServantErr (Link t m)
  , renderLinks :: Req t -> Links api t m
  }

data Runtime

instance Accept Runtime where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Runtime a where
  mimeRender _ _ = LB.toLazyByteString $ mconcat
    [ "<!DOCTYPE html>"
    , "<html><head>"
    , script "/rts.js" ""
    , script "/lib.js" ""
    , script "/out.js" ""
    , "</head><body></body>"
    , script "/runmain.js" " defer"
    , "</html>"
    ]
    where
      script :: LB.Builder -> LB.Builder -> LB.Builder
      script s attr = mconcat ["<script language=\"javascript\" src=\"", s, "\"", attr, "></script>" ]

instance MimeUnrender Runtime a where
  mimeUnrender _ = 

data AsRender (t :: *) (m :: * -> *)
instance GenericMode (AsRender t m) where
  type AsRender t m :- api = Widgets api t m

class HasRender api t m where
  type Widgets api t m :: Type
  type Links api t m :: Type
  render ::
       SupportsServantRender t m
    => Proxy api
    -> Env t m
    -> Widgets api t m
    -> Render api t m

instance (ReflectMethod method, contents ~ (c:cs), MimeUnrender c a, SupportsServantRender t m) =>
  HasRender (Verb method status contents a) t m where
  type Widgets (Verb method status contents a) t m = a -> Link t m
  type Links   (Verb method status contents a) t m = Event t () -> Link t m
  render Proxy (Env authority onFailure failUri) cb = Render widget link
    where
      widget _ total = Right . Link $ do
        url <- sample (current authority)
        res <- performOneRequest (Proxy @c) url total
        (unLink . either (onFailure . AjaxFailure) cb) res
      link req e = Link $ do
        let method = T.decodeUtf8 (reflectMethod (Proxy @method))
        reqs <- performRequestCT (Proxy @c) (req { reqMethod = method }) authority e
        return (fmap (either ((failUri,) . onFailure . AjaxFailure) (fmap cb)) reqs)

instance (ReflectMethod method, contents ~ (c:cs), MimeUnrender c a, SupportsServantRender t m) =>
  HasRender (Reflexive (Verb method status contents a)) t m where
  type Widgets (Reflexive (Verb method status contents a)) t m = ()
  type Links   (Reflexive (Verb method status contents a)) t m =
    Event t () -> m (Event t (Either T.Text a))
  render Proxy (Env authority _ _) _ = Render widget links
    where
      widget _ _ = Left (NotFound "Not a valid request")
      links req = performRequestCT' (Proxy @c) (req { reqMethod = method }) authority
      method = T.decodeUtf8 (reflectMethod (Proxy @method))

instance (HasRender a t m, HasRender b t m) => HasRender (a :<|> b) t m where
  type Widgets (a :<|> b) t m = Widgets a t m :<|> Widgets b t m
  type Links   (a :<|> b) t m = Links a t m :<|> Links b t m
  render Proxy env ~(a :<|> b) = Render (widgetA <> widgetB) (\req -> linkA req :<|> linkB req)
    where
      Render widgetA linkA = render (Proxy @a) env a
      Render widgetB linkB = render (Proxy @b) env b

instance (KnownSymbol path, HasRender api t m) => HasRender (path :> api) t m where
  type Widgets (path :> api) t m = Widgets api t m
  type Links   (path :> api) t m = Links api t m
  render Proxy env cb = Render widgets' (links . prependPathPiece (pure (Right path)))
    where
      Render widgets links = render (Proxy @api) env cb
      widgets' uri total = case unconsPathPiece uri of
        Just (piece, rest) -> case piece == path of
          True  -> widgets rest total
          False -> Left (NotFound ("Url path piece: " <> piece <> " doesnt match: " <> path))
        Nothing -> Left (NotFound "Url too short")
      path = T.pack (symbolVal (Proxy @path))

instance (ToHttpApiData a, FromHttpApiData a, HasRender api t m, SupportsServantRender t m) =>
  HasRender (Capture cap a :> api) t m where
  type Widgets (Capture cap a :> api) t m = a -> Widgets api t m
  type Links   (Capture cap a :> api) t m = Dynamic t a -> Links api t m
  render Proxy env cb = Render widgets' links'
    where
      Render widgets links = render (Proxy @api) env cb
      widgets' uri total = case unconsPathPiece uri of
        Just (piece, rest) -> case parseUrlPiece piece :: Either T.Text a of
          Right a      -> widgets rest total
          Left failure -> Left (NotFound ("Could not parse path piece becasue: " <> failure))
        Nothing -> Left (NotFound "Url too short")
      links' req val = links (prependPathPiece (fmap (Right . toUrlPiece) val) req)

instance (KnownSymbol sym, ToHttpApiData a, HasRender api t m) =>
  HasRender (Header sym a :> api) t m where
  type Widgets (Header sym a :> api) t m = Widgets api t m
  type Links   (Header sym a :> api) t m = Dynamic t (Either T.Text a) -> Links api t m
  render Proxy env cb = Render widgets links'
    where
      Render widgets links = render (Proxy @api) env cb
      links' req referer = links (prependHeader hname (fmap toUrlPiece <$> referer) req)
      hname = T.pack (symbolVal (Proxy @sym))

instance (MimeRender ct a, contents ~ (ct:cts), HasRender api t m) =>
  HasRender (ReqBody contents a :> api) t m where
  type Widgets (ReqBody contents a :> api) t m = Widgets api t m
  type Links   (ReqBody contents a :> api) t m = Dynamic t (Either T.Text a) -> Links api t m
  render Proxy env cb = Render widgets links'
    where
      Render widgets links = render (Proxy @api) env cb
      links' req bodyDyn = links (addBody (fmap makeBody bodyDyn) req)
      makeBody body = do
        a   <- body
        ctt <- (safeDecode . renderHeader . contentType) (Proxy @ct)
        t   <- (safeDecode . LB.toStrict . mimeRender (Proxy @ct)) a
        return (ctt,t)
      safeDecode = first (T.pack . show) . T.decodeUtf8'

instance (KnownSymbol sym, ToHttpApiData a, FromHttpApiData a, HasRender api t m) =>
  HasRender (QueryParam sym a :> api) t m where
  type Widgets (QueryParam sym a :> api) t m = Widgets api t m
  type Links   (QueryParam sym a :> api) t m = Dynamic t (Either T.Text a) -> Links api t m
  render Proxy env cb = Render widgets' links'
    where
      Render widgets links = render (Proxy @api) env cb
      widgets' uri total = case unconsQuery uri of
        Just (QueryPieceParam qp val,rest) -> case qp == queryPath of
          True -> case parseQueryParam val :: Either T.Text a of
            Right _      -> widgets rest total
            Left failure -> Left (NotFound ("Could not parse query param because: " <> failure))
          False -> Left (NotFound ("Query piece: " <> qp <> " did not match the expected: " <> queryPath))
        Just _  -> Left (NotFound "Query piece was a query flag")
        Nothing -> Left (NotFound "Url too short")
      links' req queryDyn = links (prependQueryParam (fmap (QueryPieceParam queryPath . toQueryParam) <$> queryDyn) req)
      queryPath = T.pack (symbolVal (Proxy @sym))
