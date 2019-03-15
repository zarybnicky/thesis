{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian, parseTimeM)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core
import Text.Read (readMaybe)

main :: IO ()
main = run 3000 $ mainWidgetWithCss css $
  divClass "content" .
  tabDisplay "menu" "selected" . mconcat . zipWith (=:) [(1 :: Int)..] $
    [ ("1) Counter", counter)
    , ("2) Temperature Converter", tempConverter)
    , ("3) Flight Booker", flightBooker)
    , ("4) Timer", timer)
    , ("5) CRUD", crud)
    , ("6) Circle Drawer", circleDrawer)
    , ("7) Cells", cells)
    ]

css :: ByteString
css = mconcat
  [ ".content { display: flex; }"
  , ".content > ul { flex: 0 0 250px; }"
  , ".content > div { flex: 1 1 auto; padding: 10px; }"
  , ".menu { list-style: none; padding: 0; border: 1px solid #eee; }"
  , ".menu li { margin: 0; border: 1px solid #eee; }"
  , ".menu li.selected { font-weight: bold }"
  , ".menu a { padding: .4rem; display: block; }"
  ]


counter :: MonadWidget t m => m ()
counter = do
  eClick <- button "Click Me"
  text "Clicks: "
  display =<< foldDyn (+) (0 :: Int) (1 <$ eClick)


doubleInput :: MonadWidget t m => Event t Double -> m (Event t Double)
doubleInput eSet = do
  inp <- inputElement $ def { _inputElementConfig_setValue = Just (fmap (T.pack . show) eSet) }
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


data FlightType = OneWay | Return
  deriving (Eq, Ord)

dateInput :: MonadWidget t m => Day -> Dynamic t Bool -> m (Dynamic t (Maybe Day))
dateInput initial disabled = do
  let initial' = T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" initial
  rec let attrs = bool mempty ("disabled" =: "disabled") <$> disabled
      let attrs' = maybe (<> "style" =: "background-color:red") (const id) <$> i' <*> attrs
      modifyAttrs <- dynamicAttributesToModifyAttributes attrs'
      i <- fmap _inputElement_value . inputElement $ def
        & inputElementConfig_initialValue .~ initial'
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
      let i' = fmap (parseTimeM True defaultTimeLocale "%d.%m.%Y" . T.unpack) i
  pure i'

flightBooker :: MonadWidget t m => m ()
flightBooker = do
  let initial = fromGregorian 2019 3 15
  typ <- value <$> dropdown OneWay (constDyn (OneWay =: "one-way flight" <> Return =: "return flight")) def
  from <- dateInput initial (constDyn True)
  to <- dateInput initial ((== Return) <$> typ)
  eClick <- button "Click Me"
  _ <- widgetHold blank $ tagPromptlyDyn (f <$> typ <*> from <*> to) eClick
  pure ()
  where
    f OneWay (Just x) _ = text "Go!"
    f Return (Just x) (Just y) = text "Go!"
    f _ _ _ = text "Error"


timer :: MonadWidget t m => m ()
timer = blank


crud :: MonadWidget t m => m ()
crud = blank


circleDrawer :: MonadWidget t m => m ()
circleDrawer = blank


cells :: MonadWidget t m => m ()
cells = blank
