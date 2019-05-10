{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.FlightBooker
  ( flightBooker
  ) where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian, parseTimeM)
import qualified Data.Text as T
import Reflex.Dom.Core

import Utils (buttonMaybe)


data FlightType
  = OneWay
  | Return
  deriving (Eq, Ord)

dateInput :: MonadWidget t m => Day -> Dynamic t Bool -> m (Dynamic t (Maybe Day))
dateInput initial disabled = do
  rec let attrs = bool mempty ("disabled" =: "disabled") <$> disabled
      let attrs' = maybe (<> "style" =: "background-color:red") (const id) <$> i' <*> attrs
      modifyAttrs <- dynamicAttributesToModifyAttributes attrs'
      i <- fmap value . inputElement $ def
        & inputElementConfig_initialValue .~ showTime initial
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
      let i' = fmap (parseTimeM True defaultTimeLocale "%d.%m.%Y" . T.unpack) i
  pure i'

flightBooker :: MonadWidget t m => m ()
flightBooker = do
  let initial = fromGregorian 2019 3 15
  typ <- value <$> dropdown OneWay (constDyn (OneWay =: "one-way flight" <> Return =: "return flight")) def
  el "br" blank
  dateFrom <- dateInput initial (constDyn False)
  el "br" blank
  dateTo <- dateInput initial ((== OneWay) <$> typ)
  el "br" blank

  let result = f <$> typ <*> dateFrom <*> dateTo
  eSubmit <- buttonMaybe result "Book flight"
  void $ widgetHold blank $ fromMaybe blank <$> leftmost [
      Nothing <$ updated result,
      Just <$> eSubmit
    ]
  where
    f OneWay (Just x) _ = Just (text $ "You have booked a one-way flight on " <> showTime x)
    f Return (Just x) (Just y)
      | x < y = Just (text $ "You have booked a return flight on " <> showTime x <> " and " <> showTime y)
      | otherwise = Nothing
    f _ _ _ = Nothing

showTime :: Day -> T.Text
showTime = T.pack . formatTime defaultTimeLocale "%d.%m.%Y"
