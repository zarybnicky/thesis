{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Tasks.Crud
  ( crud
  ) where

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import Reflex.Dom.Core

import Utils (buttonMaybe, selectViewListWithMaybeKey_)


crud :: MonadWidget t m => m ()
crud = do
  rec db <- foldDyn ($) ((0 :: Int) =: ("Emil", "Hans")) $ leftmost
        [ M.insert <$> nextId <*> current dPerson <@ eCreate
        , M.adjust . const <$> current dPerson <@> eUpdate
        , M.delete <$> eDelete
        ]
      let nextId = (+ 1) . foldr max 0 . M.keys <$> current db
      dFilter <- value <$> inputElement def
      let dbFiltered = (\f -> M.filter (T.isPrefixOf f . snd)) <$> dFilter <*> db
      dSelected <- foldDyn (\x y -> if x == y then Nothing else x) Nothing (Just <$> eSelect)
      eSelect <- elAttr "select" ("multiple" =: "multiple") $
        selectViewListWithMaybeKey_ dSelected dbFiltered personListItem
      dPerson <- personForm $ (\m -> maybe ("", "") (m M.!)) <$> current dbFiltered <@> updated dSelected
      eCreate <- button "Create"
      eUpdate <- buttonMaybe dSelected "Update"
      eDelete <- buttonMaybe dSelected "Delete"
  pure ()

personListItem :: MonadWidget t m => Dynamic t (Text, Text) -> Dynamic t Bool -> m (Event t ())
personListItem dContents dSelected = do
  (e, ()) <- elDynAttr' "option"
    (bool mempty ("selected" =: "selected") <$> dSelected)
    (dynText $ (\(name, surname) -> surname <> ", " <> name) <$> dContents)
  pure $ () <$ domEvent Click e

personForm :: MonadWidget t m => Event t (Text, Text) -> m (Dynamic t (Text, Text))
personForm eSet = do
  dName <- fmap value . inputElement $ def & inputElementConfig_setValue .~ (fst <$> eSet)
  dSurname <- fmap value . inputElement $ def & inputElementConfig_setValue .~ (snd <$> eSet)
  pure $ (,) <$> dName <*> dSurname
