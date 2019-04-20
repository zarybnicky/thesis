{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JMacro
import Network.Wai.Handler.Warp (run)
import Reflex.Dom.Core
import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Text.PrettyPrint.Leijen.Text (displayTStrict, renderPretty)
import Network.HTTP.Media ((//), (/:))

main :: IO ()
main = run 3000 $ genericServe record

record :: Routes AsServer
record = Routes
  { _root = throwError $ err301 { errHeaders = [("Location", "/index.html")] }
  , _index = liftIO . fmap (T.pack . BC.unpack . snd) . renderStatic $
      el "html" $ do
        el "head" $
          el "style" . text $ mconcat
            [ "iframe { display: block; margin: 1rem; box-shadow: 2px 2px 10px 0px #eee inset; width: 50%; }"
            , "#comparison { display: flex; direction: row; margin-bottom: 2rem; }"
            ]
        el "body" $ do
          text "This demo uses code from https://serviceworke.rs/strategy-network-or-cache.html"
          elAttr "div" ("id" =: "comparison") $ do
            elAttr "iframe" ("src" =: "./non-controlled.html" <> "id" =: "reference") blank
            elAttr "iframe" ("src" =: "./controlled.html" <> "id" =: "sample") blank
          elAttr "button" ("id" =: "reload") (text "Reload")
          el "script" $ text (jsToText appJS)
  , _control = liftIO . fmap (T.pack . BC.unpack . snd) . renderStatic $
      el "html" $
        el "body" $ elAttr "img" ("src" =: "./asset") blank
  , _noncontrol = liftIO . fmap (T.pack . BC.unpack . snd) . renderStatic $
      el "html" $
        el "body" $ elAttr "img" ("src" =: "./asset") blank
  , _sw = pure . jsToText $ sw "network-or-cache" ["./controlled.html", "./asset"]
  , _asset = liftIO $ BC.readFile "asset.png"
  }

appJS :: JStat
appJS = [jmacro|
navigator.serviceWorker.register('sw.js', { scope: './controlled' });
var referenceIframe = document.getElementById('reference');
var sampleIframe = document.getElementById('sample');
function reload() {
  referenceIframe.contentWindow.location.reload();
  sampleIframe.contentWindow.location.reload();
}
navigator.serviceWorker.ready.then(reload);
document.getElementById('reload').onclick = reload;
function fixHeight(e) {
  var iframe = e.target;
  var document = iframe.contentWindow.document.documentElement;
  iframe.style.height = document.getClientRects()[0].height + 'px';
}
referenceIframe.onload = fixHeight;
sampleIframe.onload = fixHeight;
|]

data HTML
data JS
instance Accept HTML where
   contentType _ = "text" // "html" /: ("charset", "utf-8")
instance Accept JS where
   contentType _ = "application" // "javascript" /: ("charset", "utf-8")
instance MimeRender HTML Text where
   mimeRender _  = BL.fromStrict . T.encodeUtf8
instance MimeRender JS Text where
   mimeRender _ = BL.fromStrict . T.encodeUtf8

data Routes route = Routes
    { _root :: route :- Get '[HTML] NoContent
    , _index :: route :- "index.html" :> Get '[HTML] Text
    , _control :: route :- "controlled.html" :> Get '[HTML] Text
    , _noncontrol :: route :- "non-controlled.html" :> Get '[HTML] Text
    , _sw :: route :- "sw.js" :> Get '[JS] Text
    , _asset :: route :- "asset" :> Get '[OctetStream] BC.ByteString
    }
  deriving (Generic)

jsToText :: JStat -> Text
jsToText = displayTStrict . renderPretty 1 80 . renderJs

sw :: String -> [String] -> JStat
sw cacheName prefetch =
  (if null prefetch
   then BlockStat []
   else handleInstall [jmacroE|function(evt) {
     console.log('The service worker is being installed.');
     evt.waitUntil(function () {
       return caches.open(`(cacheName)`).then(function (cache) {
         return cache.addAll(`(prefetch)`);
       });
     });
   }|]) <>
  handleFetch [jmacroE|function(evt) {
    console.log('The service worker is serving the asset.');
    evt.respondWith(fromNetwork(evt.request, 400).then(null, function () {
      return fromCache(`(cacheName)`, evt.request);
    }));
  }|]

fromNetwork :: JExpr
fromNetwork = [jmacroE|
function(request, timeout) {
  return new Promise(function (fulfill, reject) {
    var timeoutId = setTimeout(reject, timeout);
    fetch(request).then(function (response) {
      clearTimeout(timeoutId);
      fulfill(response);
    }, reject);
  });
}|]

fromCache :: JExpr
fromCache = [jmacroE|
function (cacheName, request) {
  return caches.open(cacheName).then(function (cache) {
    return cache.match(request).then(function (matching) {
      return matching || Promise.reject('no-match');
    });
  });
}|]

handleInstall :: JExpr -> JStat
handleInstall fn = [jmacro|self.addEventListener('install', `(fn)`);|]

handleFetch :: JExpr -> JStat
handleFetch fn = [jmacro|self.addEventListener('fetch', `(fn)`);|]
