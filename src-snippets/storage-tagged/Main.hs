{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens hiding (set)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Storage (getItem, setItem)
import GHCJS.DOM.Window (getLocalStorage)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidget $ mdo
  val0 <- maybe "Nothing" unVal <$> getTagged
  val <- holdDyn val0 eVal
  let eVal = current (value inp) <@ keypress Enter inp
  performEvent_ $ setTagged . Val <$> eVal

  pb <- getPostBuild
  inp <- textInput $ def & setValue .~ (val0 <$ pb)
  el "br" blank
  dynText val

get :: forall (tag :: Symbol) a m. (KnownSymbol tag, FromJSON a, MonadJSM m) => m (Maybe a)
get = liftJSM $ do
  storage <- getLocalStorage =<< currentWindowUnchecked
  val <- getItem storage (symbolVal $ Proxy @tag)
  pure $ decode . BC.pack . T.unpack =<< val

set :: forall (tag :: Symbol) a m. (KnownSymbol tag, ToJSON a, MonadJSM m) => a -> m ()
set a = liftJSM $ do
  storage <- getLocalStorage =<< currentWindowUnchecked
  setItem storage (symbolVal $ Proxy @tag) . T.pack . BC.unpack $ encode a

class (FromJSON a, ToJSON a, KnownSymbol (StoreTag a)) => Stored a where
  type StoreTag a :: Symbol

newtype Val = Val
  { unVal :: Text
  } deriving (FromJSON, ToJSON)

instance Stored Val where
  type StoreTag Val = "stuff"

getTagged :: forall a m. (Stored a, MonadJSM m) => m (Maybe a)
getTagged = get @(StoreTag a)

setTagged :: forall a m. (Stored a, MonadJSM m) => a -> m ()
setTagged = set @(StoreTag a)
