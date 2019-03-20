{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( buttonMaybe
  , selectViewListWithMaybeKey_
  , tshow
  ) where

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import Reflex.Dom.Core


tshow :: Show t => t -> Text
tshow = T.pack . show

buttonMaybe :: MonadWidget t m => Dynamic t (Maybe a) -> Text -> m (Event t a)
buttonMaybe dDisabled txt = do
  let attrs = maybe ("disabled" =: "disabled") (const mempty) <$> dDisabled
  (e, ()) <- elDynAttr' "button" attrs (text txt)
  pure . fmapMaybe id $ current dDisabled <@ domEvent Click e

selectViewListWithMaybeKey_ ::
     (MonadWidget t m, Ord k)
  => Dynamic t (Maybe k)
  -> Dynamic t (M.Map k v)
  -> (Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -> m (Event t k)
selectViewListWithMaybeKey_ selection vals mkChild = do
  let selectionDemux = demux selection
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux (Just k)
    fmap (k <$) (mkChild v selected)
  pure $ switchPromptlyDyn $ leftmost . M.elems <$> selectChild
