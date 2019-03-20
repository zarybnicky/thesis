{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.TempConverter
  ( tempConverter
  ) where

import Reflex.Dom.Core
import qualified Data.Text as T
import Text.Read (readMaybe)

import Utils (tshow)


doubleInput :: MonadWidget t m => Event t Double -> m (Event t Double)
doubleInput eSet = do
  inp <- inputElement $ def { _inputElementConfig_setValue = Just (tshow <$> eSet) }
  pure $ fmapMaybe (readMaybe . T.unpack) (_inputElement_input inp)

tempConverter :: MonadWidget t m => m ()
tempConverter = do
  rec celsius <- doubleInput (f2c <$> fahrenheit)
      text " Celsius = "
      fahrenheit <- doubleInput (c2f <$> celsius)
  text " Fahrenheit"
  where
    f2c, c2f :: Double -> Double
    f2c x = (x - 32) * 5 / 9
    c2f x = x * 9 / 5 + 32
