{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.Cells
  ( cells
  ) where

import Reflex.Dom.Core


cells :: MonadWidget t m => m ()
cells = blank
