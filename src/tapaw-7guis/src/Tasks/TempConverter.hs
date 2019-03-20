{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.TempConverter
  ( tempConverter
  ) where

import Reflex.Dom.Core
import Utils (doubleInput)

tempConverter :: MonadWidget t m => m ()
tempConverter = do
  rec celsius <- doubleInput Nothing (f2c <$> fahrenheit)
      text " Celsius = "
      fahrenheit <- doubleInput Nothing (c2f <$> celsius)
  text " Fahrenheit"
  where
    f2c, c2f :: Double -> Double
    f2c x = (x - 32) * 5 / 9
    c2f x = x * 9 / 5 + 32
