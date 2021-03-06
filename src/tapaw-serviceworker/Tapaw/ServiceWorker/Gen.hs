{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Tapaw.ServiceWorker.Gen
  ( CacheStrategy(..)
  , PushBehavior(..)
  , RequestMatcher(..)
  , MethodMatcher(..)
  , QueryMatcher(..)
  , PathMatcher(..)
  , PathComponentMatcher(..)
  , ValueMatcher(..)
  , ServiceWorker(..)
  , generateWorker
  , renderFetchMatchers
  , renderMethodMatcher
  , renderQueryMatcher
  , renderPathMatcher
  , renderPathComponentMatcher
  , renderValueMatcher
  , renderCacheStrategy
  , renderPushBehavior
  , matchPath
  , matchSegment
  , matchNumeric
  ) where

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.Javascript.JMacro
  (JExpr(..), JStat(..), JVal(..), jLam, jVarTy, jhFromList, jmacro, jmacroE, jsv,
   renderJs, toJExpr, toStat)
import Text.PrettyPrint.Leijen.Text (displayTStrict, renderPretty)

data ServiceWorker push = ServiceWorker
  { swPrecache :: [Text]
  , swFetch :: [(RequestMatcher, CacheStrategy)]
  , swPush :: PushBehavior push
  }

generateWorker :: ServiceWorker push -> ByteString
generateWorker ServiceWorker{..} = BC.pack . T.unpack . displayTStrict . renderPretty 1 80 . renderJs $ [jmacro|
  self.addEventListener('install', function (e) {
    e.waitUntil(caches.open('precache').then(function (cache) {
      return cache.addAll(`(swPrecache)`);
    }));
  });
  self.addEventListener('fetch', function (e) {
    var req = e.request;
    var method = req.method;
    var url = new URL(req.url);
    var path = url.pathname.substr(1).split('/');
    var qs = url.searchParams;
    `(renderFetchMatchers (e, req) (method, path, qs) swFetch)`
  });
  `(renderPushBehavior swPush)`
|]

data RequestMatcher = RequestMatcher
  { rmMethod :: MethodMatcher
  , rmQuery :: QueryMatcher
  , rmPath :: PathMatcher
  }

matchPath :: PathComponentMatcher -> RequestMatcher
matchPath = RequestMatcher MethodAny (QueryMatcher []) . PathComponentMatcher

matchSegment :: Text -> PathComponentMatcher -> PathComponentMatcher
matchSegment x = PathComponent (MatchStatic x)

matchNumeric :: PathComponentMatcher -> PathComponentMatcher
matchNumeric = PathComponent MatchNumeric

renderFetchMatchers :: (JExpr, JExpr) -> (JExpr, JExpr, JExpr) -> [(RequestMatcher, CacheStrategy)] -> JStat
renderFetchMatchers (evt, req) (method, path, qs) = mconcat . fmap (\(RequestMatcher{..}, strategy) -> [jmacro|
  if (`(renderMethodMatcher method rmMethod)` &&
      `(renderQueryMatcher qs rmQuery)` &&
      `(renderPathMatcher path req rmPath)`) {
    `(renderCacheStrategy evt req strategy)`
  }
|])

data MethodMatcher
  = MethodAny
  | MethodList [Text]

renderMethodMatcher :: JExpr -> MethodMatcher -> JExpr
renderMethodMatcher method = \case
  MethodAny -> jsv "true"
  MethodList ms -> [jmacroE|`(ms)`.some(\y -> `(method)` == y)|]

newtype QueryMatcher = QueryMatcher [(Text, ValueMatcher)]

renderQueryMatcher :: JExpr -> QueryMatcher -> JExpr
renderQueryMatcher qs (QueryMatcher ms) = foldl (InfixExpr "&&") (jsv "true") $ flip fmap ms $
  \(k, vm) -> renderValueMatcher [jmacroE|`(qs)`.get(`(k)`)|] vm

data PathMatcher
  = PathComponentMatcher PathComponentMatcher
  | PathRegexMatcher Text

renderPathMatcher :: JExpr -> JExpr -> PathMatcher -> JExpr
renderPathMatcher path req = \case
  PathRegexMatcher r -> [jmacroE|`(JRegEx (T.unpack r))`.test(`req`.url)|]
  PathComponentMatcher pc -> renderPathComponentMatcher 0 path pc

data PathComponentMatcher
  = PathMatchAny
  | PathMatchEnd
  | PathComponent ValueMatcher PathComponentMatcher

renderPathComponentMatcher :: Int -> JExpr -> PathComponentMatcher -> JExpr
renderPathComponentMatcher i e = \case
  PathMatchAny -> jsv "true"
  PathMatchEnd -> [jmacroE|!`(e)`[`(i)`]|]
  PathComponent vm rest ->
    InfixExpr "&&" (renderValueMatcher [jmacroE|`(e)`[`(i)`]|] vm) (renderPathComponentMatcher (i + 1) e rest)

data ValueMatcher
  = MatchAny
  | MatchNumeric
  | MatchStatic Text
  | MatchRegex Text

renderValueMatcher :: JExpr -> ValueMatcher -> JExpr
renderValueMatcher e = \case
  MatchAny -> jsv "true"
  MatchNumeric -> [jmacroE|isNumeric(`(e)`)|]
  MatchStatic x -> [jmacroE|(`(x)` == `(e)`)|]
  MatchRegex x -> [jmacroE|(`(JRegEx (T.unpack x))`.test(`(e)`))|]

data CacheStrategy
  = CacheFirst Text
  | CacheOnly Text
  | NetworkFirst Text Int
  | NetworkOnly
  | StaleWhileRevalidate Text
  deriving (Eq, Ord, Show)

renderCacheStrategy :: JExpr -> JExpr -> CacheStrategy -> JStat
renderCacheStrategy evt req = \case
  CacheFirst c -> [jmacro|
    return `(evt)`.respondWith(caches.open(`(c)`).then(function (cache) {
      return cache.match(`(req)`).then(function (res) {
        return res || fetch(`(req)`).then(function (res2) {
          `(evt)`.waitUntil(cache.put(`(req)`, res2.clone()));
          return res2;
        });
      });
    }));
  |]
  CacheOnly c -> [jmacro|
    return `(evt)`.respondWith(caches.open(`(c)`).then(function (cache) {
      return cache.match(`(req)`)
    }));
  |]
  NetworkFirst c timeout -> [jmacro|
    var network = fetch(`(req)`).then(function (res) {
      return caches.open(`(c)`).then(function (cache) {
        `(evt)`.waitUntil(cache.put(`(req)`, res.clone()));
        return res;
      });
    });
    var cached = new Promise(function (resolve) {
      setTimeout(function () {
        resolve(caches.open(`(c)`).then(function (cache) {
          return cache.match(`(req)`);
        }));
      }, `(timeout)`);
    });
    return `(evt)`.respondWith(Promise.race([network, cached]).then(function (res) {
      return res || network;
    }));
  |]
  NetworkOnly -> [jmacro|return fetch(`(req)`)|]
  StaleWhileRevalidate c -> [jmacro|
    var network = fetch(`(req)`).then(function (res) {
      return caches.open(`(c)`).then(function (cache) {
        return cache.put(`(req)`, res.clone());
      }).then(function () {
        return res;
      });
    }, function () {
      //Ignore errors
      return undefined;
    });
    `(evt)`.waitUntil(network);
    return `(evt)`.respondWith(caches.open(`(c)`).then(function (cache) {
      return cache.match(`(req)`);
    }).then(function (res) {
      return res || network;
    }));
  |]

data PushBehavior a where
  PushIgnore :: PushBehavior Void
  PushViewOnly :: PushBehavior ()
  PushViewAndOpen :: Text -> PushBehavior ()
  PushViewAndProcess :: FromJSON a => PushBehavior a
  PushProcessOnly :: FromJSON a => PushBehavior a

renderPushBehavior :: PushBehavior a -> JStat
renderPushBehavior = \case
  PushIgnore -> mempty
  PushViewOnly -> [jmacro|
    self.addEventListener('push', function (e) {
      var x = `(e)`.data.json();
      `(e)`.waitUntil(self.registration.showNotification(x.title, { body: x.body }));
    });
  |]
  PushViewAndOpen url -> [jmacro|
    self.addEventListener('push', function (e) {
      var x = `(e)`.data.json();
      `(e)`.waitUntil(self.registration.showNotification(x.title, { body: x.body }));
    });
    self.addEventListener('notificationclick', function (e) {
      e.waitUntil(self.clients.matchAll().then(function (clients) {
        if (clients.length > 0) {
          return clients[0].focus();
        }
        return self.clients.openWindow(`(url)`);
      }));
    });
  |]
  PushViewAndProcess -> [jmacro|
    self.addEventListener('push', function (e) {
      var x = `(e)`.data.json();
      `(e)`.waitUntil(self.clients.matchAll().then(function (clients) {
        clients.forEach(function (client) {
          client.postMessage(x);
        });
      }).then(function () {
        self.registration.showNotification(x.title, { body: x.body })
      }));
    });
  |]
  PushProcessOnly -> [jmacro|
    self.addEventListener('push', function (e) {
      var x = `(e)`.data.json();
      `(e)`.waitUntil(self.clients.matchAll().then(function (clients) {
        clients.forEach(function (client) {
          client.postMessage(x);
        });
      }));
    });
  |]
