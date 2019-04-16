{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Render.Client
  ( serve
  ) where

import Data.Proxy (Proxy)
import Reflex.Class (MonadHold(..), Reflex(..))
import Reflex.Dom.Core (DomBuilder, PostBuild, dyn)
import Control.Monad.Fix (MonadFix)
import Servant.Common.Uri (Authority(..), Uri(..))
import Servant.Common.PopState (url)
import Servant.Render
  ( Env(..)
  , HasRender(..)
  , Link
  , Render(..)
  , ServantErr
  , SupportsServantRender
  , linkView
  )

serve ::
     forall api t m.
     ( HasRender api t m
     , MonadFix m
     , MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , SupportsServantRender t m
     )
  => Proxy api
  -> Dynamic t Authority
  -> Widgets api t m
  -> Uri
  -> (ServantErr -> Link t m)
  -> m (Event t Uri)
serve api authority makeWidgets errorPageLoc makeErrorPage = do
  let (Render widgets makeLinks) = render api (Env authority errorPage errorPageLoc) makeWidgets :: Render api t m
      errorPage  = makeErrorPage
      double f x = f x x
  rec urls       <- url authority newUrls
      changePage <- dyn (fmap (linkView . either errorPage id . double widgets) urls)
      newUrls    <- fmap switch (hold never changePage)
  return newUrls
