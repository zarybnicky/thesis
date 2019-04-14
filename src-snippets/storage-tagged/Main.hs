{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens hiding (set)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Result(..), fromJSON, toJSON)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.IORef (IORef(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.IDBDatabase as IDB
import qualified GHCJS.DOM.IDBFactory as IDB
import qualified GHCJS.DOM.IDBRequest as IDB
import qualified GHCJS.DOM.IDBObjectStore as IDB
import GHCJS.DOM.Types (IDBObjectStoreParameters(..), IDBRequestResult(..))
import GHCJS.DOM.Window (getLocalStorage, getIndexedDB)
import Language.Javascript.JSaddle
  ( JSException
  , JSString
  , JSVal(..)
  , MonadJSM
  , (!)
  , catch
  , fromJSVal
  , js1
  , js2
  , jsg
  , jss
  , liftJSM
  , maybeNullOrUndefined
  , toJSVal
  )
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core hiding (Error)

main :: IO ()
main = run 3000 $ mainWidget $ mdo
  val0 <- getTagged
  dVal <- holdDyn val0 (Just <$> eVal)
  let eVal = User <$> current (value inp) <*> current (value inp2) <@ leftmost [keypress Enter inp, keypress Enter inp2]
  performEvent_ $ setTagged <$> eVal

  pb <- getPostBuild
  inp <- textInput $ def & setValue .~ (maybe "" name val0 <$ pb)
  inp2 <- textInput $ def & setValue .~ (maybe "" surname val0 <$ pb)
  el "br" blank
  display dVal
  w <- currentWindowUnchecked
  performEvent_ $ (\x -> liftJSM $ w ^. jss ("x" :: JSString) x) . toJSON <$> eVal

  liftJSM $ initObjectStore ("db", Just 1) "table"

get :: forall (tag :: Symbol) a m. (KnownSymbol tag, FromJSON a, MonadJSM m) => m (Maybe a)
get = liftJSM $ do
  storage <- getLocalStorage =<< currentWindowUnchecked
  storage ! symbolVal (Proxy @tag) >>= maybeNullOrUndefined >>= \case
    Nothing -> pure Nothing
    Just v -> do
      json <- jsg ("JSON" :: JSString)
      v' <- catch (fromJSVal =<< json ^. js1 ("parse" :: JSString) v) (\(_ :: JSException) -> pure Nothing)
      pure $ resultToMaybe . fromJSON =<< v'
  where
    resultToMaybe (Success a) = Just a
    resultToMaybe (Error _) = Nothing

set :: forall (tag :: Symbol) a m. (KnownSymbol tag, ToJSON a, MonadJSM m) => a -> m ()
set a = liftJSM $ do
  storage <- getLocalStorage =<< currentWindowUnchecked
  json <- jsg ("JSON" :: JSString)
  val <- json ^. js1 ("stringify" :: JSString) (toJSON a)
  _ <- storage ^. js2 ("setItem" :: JSString) (symbolVal (Proxy @tag)) val
  pure ()

class (FromJSON a, ToJSON a, KnownSymbol (StoreTag a)) => Stored a where
  type StoreTag a :: Symbol

data User = User
  { name :: Text
  , surname :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

instance Stored User where
  type StoreTag User = "currentUser"

getTagged :: forall a m. (Stored a, MonadJSM m) => m (Maybe a)
getTagged = get @(StoreTag a)

setTagged :: forall a m. (Stored a, MonadJSM m) => a -> m ()
setTagged = set @(StoreTag a)


initObjectStore :: MonadJSM m => (Text, Maybe Word64) -> Text -> m ()
initObjectStore (dbName, dbVersion) tableName = liftJSM $ do
  dbFactory <- getIndexedDB =<< currentWindowUnchecked
  c <- jsg ("console" :: JSString)
  openReq <- IDB.open dbFactory dbName dbVersion
  void . on openReq IDB.success $ do
    db <- IDB.getResultUnsafe openReq
    params <- liftJSM . toJSVal . toJSON $ ("keyPath" :: Text) =: ("id" :: Text)
    _ <- liftJSM $ c ^. js1 ("log" :: JSString) db
    os <- IDB.createObjectStore (coerce db) tableName (Just $ IDBObjectStoreParameters params)
    _ <- liftJSM $ c ^. js1 ("log" :: JSString) os
    pure ()

  -- TODO: onVersionChange
