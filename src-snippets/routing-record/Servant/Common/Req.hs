{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Servant.Common.Req
  ( Req(..)
  , SupportsServantRender
  , performOneRequest
  , performRequest
  , performRequestCT
  , performRequestCT'
  , defReq
  , prependPathPiece
  , prependQueryParam
  , prependHeader
  , addBody
  ) where

import Control.Applicative ((<|>), liftA2)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as LB
import Data.Bool (bool)
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom.Core
import Servant.API (BasicAuthData(..), MimeUnrender(..))
import Servant.Common.Uri (Authority(..), QueryPiece(..), Uri(..), encodeUrl)


decodeXhrRes :: (MimeUnrender c a) => Proxy c -> XhrResponse -> Either T.Text a
decodeXhrRes ct xhrRes =
  maybe (Left "No body text") Right (_xhrResponse_responseText xhrRes) >>=
  first T.pack . mimeUnrender ct . LB.fromStrict . T.encodeUtf8

performOneRequest :: (MimeUnrender c a, SupportsServantRender t m) =>
  Proxy c -> Authority -> Uri -> m (Either T.Text a)
performOneRequest ct authority uri = do
  var <- liftIO newEmptyMVar
  _ <- newXMLHttpRequest (xhrRequest "GET" (encodeUrl authority uri) headers) (liftIO . putMVar var)
  liftIO (decodeXhrRes ct <$> takeMVar var)
  where headers = def { _xhrRequestConfig_headers = "Accept" =: "application/json" }

-------------------------------------------------------------------------------
-- The data structure used to build up request information while traversing
-- the shape of a servant API
data Req t = Req
  { reqMethod      :: T.Text
  , reqPathParts   :: [Dynamic t (Either T.Text T.Text)]
  , reqQueryParams :: [Dynamic t (Either T.Text QueryPiece)]
  , reqBody        :: Maybe (Dynamic t (Either T.Text (T.Text, T.Text)))
  , reqHeaders     :: [(T.Text, Dynamic t (Either T.Text T.Text))]
  , reqRespHeaders :: XhrResponseHeaders
  , reqAuthData    :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependPathPiece :: Dynamic t (Either T.Text T.Text) -> Req t -> Req t
prependPathPiece p req =
  req { reqPathParts = p : reqPathParts req }

prependQueryParam :: Dynamic t (Either T.Text QueryPiece) -> Req t -> Req t
prependQueryParam q req =
  req { reqQueryParams = q : reqQueryParams req }

prependHeader :: T.Text -> Dynamic t (Either T.Text T.Text) -> Req t -> Req t
prependHeader name referer req = req { reqHeaders = (name,referer) : reqHeaders req }

addBody :: Dynamic t (Either T.Text (T.Text, T.Text)) -> Req t -> Req t
addBody body req = req { reqBody = reqBody req <|> Just body }

type SupportsServantRender t m
   = ( Reflex t
     , MonadJSM m
     , HasJSContext m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , MonadSample t m
     , TriggerEvent t m
     , PerformEvent t m
     )

performRequest ::
     forall t m. (SupportsServantRender t m)
  => Req t
  -> Dynamic t Authority
  -> Event t ()
  -> m (Event t (Either T.Text (Uri, XhrResponse)))
performRequest req authority trigger =
  fmap getCompose <$> performRequestsAsync (Compose <$> tagPromptlyDyn (liftAA2 (,) uri reqs) trigger)
  where
    liftAA2 = liftA2 . liftA2

    reqs :: Dynamic t (Either T.Text (XhrRequest T.Text))
    reqs = liftAA2 (XhrRequest (reqMethod req)) (fmap . encodeUrl <$> authority <*> uri) config

    uri :: Dynamic t (Either T.Text Uri)
    uri = liftAA2 Uri (pieces (reqPathParts req)) (pieces (reqQueryParams req))
      where pieces = fmap (fmap reverse . sequence) . sequence

    config :: Dynamic t (Either T.Text (XhrRequestConfig T.Text))
    config = do
      headersE    <- headersD
      authInfoE   <- authInfoD
      bodyE       <- bodyD
      return $ do
        headers     <- headersE
        (user,pass) <- authInfoE
        (ct,body)   <- bodyE
        let m = M.fromList headers
            headers' = bool (M.insert "Content-Type" ct m) m (T.null ct)
        return (XhrRequestConfig headers' user pass Nothing (fromMaybe "" body) False def)
    headersD :: Dynamic t (Either T.Text [(T.Text, T.Text)])
    headersD = (fmap sequence . traverse tup . reqHeaders) req
      where
        tup (headerName,d) = fmap (headerName,) <$> d

    authInfoD :: Dynamic t (Either T.Text (Maybe T.Text, Maybe T.Text))
    authInfoD = case reqAuthData req of
      Just authD -> ffor authD $ \case
        Just (BasicAuthData user pass) -> do
          userE <- safeDecode user
          passE <- safeDecode pass
          return (Just userE,Just passE)
        Nothing -> Right (Nothing, Nothing)
      Nothing -> pure $ Right (Nothing, Nothing)
      where
        safeDecode = first (T.pack . show) . T.decodeUtf8'

    bodyD :: Dynamic t (Either T.Text (T.Text, Maybe T.Text))
    bodyD = case reqBody req of
      Just body -> (fmap . fmap . fmap) Just body
      Nothing -> pure (Right ("", Nothing))

performRequestCT ::
     (MimeUnrender c a, SupportsServantRender t m)
  => Proxy c
  -> Req t
  -> Dynamic t Authority
  -> Event t ()
  -> m (Event t (Either T.Text (Uri, a)))
performRequestCT ct req authority =
  fmap (fmap parseRes) . performRequest req authority
  where
    parseRes res = do
      (uri, xhr) <- res
      a <- decodeXhrRes ct xhr
      return (uri, a)

performRequestCT' ::
     (MimeUnrender c a, SupportsServantRender t m)
  => Proxy c
  -> Req t
  -> Dynamic t Authority
  -> Event t ()
  -> m (Event t (Either T.Text a))
performRequestCT' ct req authority =
  fmap (fmap parseRes) . performRequest req authority
  where
    parseRes res = res >>= decodeXhrRes ct . snd
