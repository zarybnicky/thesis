{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tasks.Counter
  ( counter
  ) where

import Reflex.Dom.Core


counter :: MonadWidget t m => m ()
counter = do
  eClick <- button "Click Me"
  text "Clicks: "
  display =<< foldDyn (+) (0 :: Int) (1 <$ eClick)
