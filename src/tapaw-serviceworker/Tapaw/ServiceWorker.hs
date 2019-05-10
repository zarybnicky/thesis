{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Tapaw.ServiceWorker
  ( generateWorker
  ) where

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.Javascript.JMacro
  ( JExpr(..)
  , JStat(..)
  , JVal(..)
  , jLam
  , jVarTy
  , jmacro
  , jmacroE
  , jsv
  , renderJs
  , toJExpr
  , toStat
  )
import Text.PrettyPrint.Leijen.Text (displayTStrict, renderPretty)

data CacheStrategy
  = CacheFirst Text
  | CacheOnly Text
  | NetworkFirst Text
  | NetworkOnly
  | StaleWhileRevalidate Text
  | StaleWhileRevalidateNotify Text
  deriving (Eq, Ord, Show)
-- notify on a fixed channel via postMessage

data PushBehavior a where
  PushIgnore :: PushBehavior Void
  PushViewOnly :: PushBehavior ()
  PushViewAndOpen :: PushBehavior ()
  PushViewAndProcess :: FromJSON a => PushBehavior a
  PushProcessOnly :: FromJSON a => PushBehavior a

data RequestMatcher = RequestMatcher
  { rmMethod :: MethodMatcher
  , rmPath :: PathMatcher
  , rmQuery :: QueryMatcher
  }

data MethodMatcher
  = MethodAny
  | MethodList [Text]

newtype QueryMatcher = QueryMatcher [(Text, ValueMatcher)]

data ValueMatcher
  = MatchAny
  | MatchNumeric
  | MatchStatic Text
  | MatchRegex Text

data PathMatcher
  = PathComponentMatcher PathComponentMatcher
  | PathRegexMatcher Text

data PathComponentMatcher
  = PathAny
  | PathStop
  | PathComponent ValueMatcher PathComponentMatcher

data Loc = Loc
  { locPath :: [Text]
  , locQueryString :: [(Text, Text)]
  }

data ServiceWorker push = ServiceWorker
  { swPrecache :: (Text, [Text])
  , swFetch :: [(RequestMatcher, CacheStrategy)]
  , swPush :: PushBehavior push
  }

generateWorker :: ServiceWorker push -> ByteString
generateWorker ServiceWorker{..} = BC.pack . T.unpack . displayTStrict . renderPretty 1 80 . renderJs $ [jmacro|
  self.addEventListener('install', \e -> e.waitUntil(
    (`(generatePrefetch swPrecache)`)()
  ));
  self.addEventListener('fetch', function (e) {
     var req = e.request;
     var method = req.method;
     var url = new URL(req.url);
     var path = url.pathname.substr(1).split('/');
     var qs = url.searchParams;
     `(generateMatchers req (method, path, qs) swFetch)`
   });
|]

generateMatchers :: JExpr -> (JExpr, JExpr, JExpr) -> [(RequestMatcher, CacheStrategy)] -> JStat
generateMatchers req loc = mconcat . fmap (\(match, strategy) ->
  [jmacro|if (`(renderReqMatcher match loc)`) { return `(renderStrategy strategy req)` }|])

renderReqMatcher :: RequestMatcher -> (JExpr, JExpr, JExpr) -> Bool
renderReqMatcher m (method, url, qs) = undefined

renderStrategy :: CacheStrategy -> JExpr -> JExpr
renderStrategy s req = undefined

generatePrefetch :: (Text, [Text]) -> JExpr
generatePrefetch (cacheName, urls) = [jmacroE|
  function () {
    return caches.open(`(cacheName)`).then(function (cache) {
      return cache.addAll(`(urls)`);
    });
  }
|]
