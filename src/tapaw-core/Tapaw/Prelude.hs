module Tapaw.Prelude
  ( module X
  , (<<$>>)
  , (<<$)
  ) where

import Control.Lens as X hiding (element)
import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Trans.Class as X (lift)
import Data.Bool as X
import Data.Bifunctor as X
import Data.Default as X
import Data.Either as X
import Data.Functor as X
import Data.Foldable as X (traverse_)
import Data.Generics.Product as X (field)
import Data.Generics.Sum as X (_Ctor)
import Data.Map as X (Map)
import Data.Maybe as X
import Data.Semigroup as X (Semigroup(..))
import Data.Proxy as X (Proxy(..))
import Data.Text as X (Text)
import Data.Traversable as X
import Data.Tuple as X
import GHC.Generics as X (Generic)
import Language.Javascript.JSaddle as X (MonadJSM, liftJSM)
import Reflex.Dom.Core as X


infixl 4 <<$>>, <<$

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
v <<$ f = fmap (v <$) f
