{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Unsafe.Coerce (unsafeCoerce)
import Data.FileEmbed (embedFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM.HTMLElement (HTMLElement(..), focus)
import GHCJS.DOM.Types (MonadJSM)
import Language.Javascript.JSaddle.Warp (run)
import Tapaw.Servant (MonadRouted, appLink', runRouter)
import Tapaw.Storage (MonadKVStore(..))
import Reflex.Dom.Core

import Types (AppT(..), AppRoute(..), Task(..), TaskFilter(..), runAppT)


taskFilter :: TaskFilter -> Map Int Task -> Map Int Task
taskFilter FilterAll = id
taskFilter FilterActive = M.filter (not . completed)
taskFilter FilterCompleted = M.filter completed

main :: IO ()
main = run 3000 $ mainWidgetWithCss $(embedFile "src/index.css") (runAppT app)

app :: MonadWidget t m => AppT t m ()
app = do
  eFilter <- runRouter
    (AppRoute (pure FilterAll) (pure FilterAll) (pure FilterAll) (pure FilterAll))
    (const $ pure FilterAll)
  dFilter <- holdDyn FilterAll eFilter
  dTasks <- getKVAll @Task

  elClass "section" "todoapp" $ do
    eNewItem <- fmap (flip Task False) <$> inputBox
    let bNextIdx = (+ 1) . M.foldlWithKey (\a b _ -> max a b) (-1) <$> current dTasks
    (eToggleAll, eEdit) <- taskList (taskFilter <$> dFilter <*> dTasks)
    eClearCompleted <- navigation dFilter dTasks
    putKV eEdit
    putKV $ (\k v -> (k, Just v)) <$> bNextIdx <@> eNewItem
    putKVAll $ flip ($) <$> current dTasks <@> leftmost
      [ toggleAll <$ eToggleAll
      , M.filter (not . completed) <$ eClearCompleted
      ]
  infoFooter

toggleAll :: Map Int Task -> Map Int Task
toggleAll m =
  if not (all completed m)
    then M.map (\t -> t {completed = True}) m
    else M.map (\t -> t {completed = False}) m

navigation ::
     (DomBuilder t m, PostBuild t m, MonadRouted AppRoute t m)
  => Dynamic t TaskFilter
  -> Dynamic t (Map Int Task)
  -> m (Event t ())
navigation dFilter dTasks =
  elDynAttr "footer"
      (("class" =: "footer" <>) . bool ("style" =: "display:none") mempty . (0 <) . M.size <$> dTasks) $ do
    elClass "span" "todo-count" $ do
      el "strong" (display dNumCompleted)
      dynText $ bool " items left" " item left" . (== 1) <$> dNumCompleted
    elClass "ul" "filters" $ do
      el "li" $ appLink' rAll () (bool mempty ("class" =: "selected") . (== FilterAll) <$> dFilter)
        (pure True) (text "All")
      el "li" $ appLink' rActive () (bool mempty ("class" =: "selected") . (== FilterActive) <$> dFilter)
        (pure True) (text "Active")
      el "li" $ appLink' rCompleted () (bool mempty ("class" =: "selected") . (== FilterCompleted) <$> dFilter)
        (pure True) (text "Completed")

    (btn, ()) <- elDynAttr' "button"
      (("class" =: "clear-completed" <>) . bool ("style" =: "display:none") mempty . (0 <) <$> dNumCompleted)
      (text "Clear completed")
    pure (domEvent Click btn)
  where
    dNumCompleted = length . filter completed . M.elems <$> dTasks

taskList ::
     ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Map Int Task)
  -> m (Event t (), Event t (Int, Maybe Task))
taskList dTasks =
  elClass "section" "main" $ do
    let dAllDone = all completed . M.elems <$> dTasks
    iAllDone <- sample (current dAllDone)
    eToggleAll <- fmap _inputElement_input . inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
         ("type" =: "checkbox" <> "id" =: "toggle-all" <> "class" =: "toggle-all" <>
          bool mempty ("checked" =: "checked") iAllDone)
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ (("checked" =:) . bool Nothing (Just "checked") <$> updated dAllDone)
    elAttr "label" ("for" =: "toggle-all") (text "Mark all as complete")

    eEdit <- elDynAttr "ul"
      (("class" =: "todo-list" <>) . bool mempty ("style" =: "display:none") . M.null <$> dTasks)
      (selectViewListWithKey (pure 0) dTasks taskItem)

    pure (() <$ eToggleAll, eEdit)

taskItem ::
     ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadHold t m
     , MonadFix m
     )
  => Int
  -> Dynamic t Task
  -> Dynamic t Bool
  -> m (Event t (Maybe Task))
taskItem _ dValue _ = do
  let dChecked = completed <$> dValue
      dText = title <$> dValue
  rec
    dEditing <- holdDyn False (leftmost [False <$ eUpdated, True <$ eStartEdit])
    let liClass = (("class" =:) .) . (<>) <$>
          (bool "" "completed " <$> dChecked) <*>
          (bool "" "editing " <$> dEditing)
    (eToggle, eStartEdit, eDestroy, eUpdated) <- elDynAttr "li" liClass $ do
      (eToggle', eStartEdit', eDestroy') <- taskView dChecked dText
      eUpdated' <- taskEdit dText eStartEdit'
      pure (eToggle', eStartEdit', eDestroy', eUpdated')
  pure $ leftmost
    [ (Just .) . flip Task <$> current dChecked <@> eUpdated
    , (Just .) . Task <$> current dText <@> eToggle
    , Nothing <$ eDestroy
    ]

taskView ::
     (DomBuilder t m, PostBuild t m)
  => Dynamic t Bool
  -> Dynamic t Text
  -> m (Event t Bool, Event t (), Event t ())
taskView dChecked dText =
  divClass "view" $ do
    checkbox' <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("type" =: "checkbox" <> "class" =: "toggle")
      & inputElementConfig_setChecked .~ updated dChecked
    (label, ()) <- el' "label" (dynText dText)
    (btn, ()) <- elAttr' "button" ("class" =: "destroy") blank
    pure ( _inputElement_checkedChange checkbox'
         , () <$ domEvent Dblclick label
         , domEvent Click btn
         )

taskEdit ::
     ( DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadFix m
     )
  => Dynamic t Text
  -> Event t ()
  -> m (Event t Text)
taskEdit dText eToggle = do
  rec
    textbox <- inputElement $ def
      & inputElementConfig_setValue .~ (current dText <@ eToggle <> keydown Escape textbox)
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "edit"
  let textbox' = _element_raw $ _inputElement_element textbox
  eDelayed <- delay 0.05 eToggle
  performEvent_ $ focus (HTMLElement $ unsafeCoerce textbox') <$ eDelayed
  pure $ leftmost
    [ current (value textbox) <@ keypress Enter textbox <> domEvent Blur textbox
    , current dText <@ keydown Escape textbox
    ]

inputBox :: (DomBuilder t m, MonadFix m) => m (Event t Text)
inputBox =
  elClass "header" "header" $ do
    el "h1" (text "todos")
    rec
      textbox <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("class" =: "new-todo" <> "autofocus" =: "autofocus" <>
           "placeholder" =: "What needs to be done?")
        & inputElementConfig_setValue .~ ("" <$ keypress Enter textbox)
    pure . ffilter T.null $ T.strip <$> current (value textbox) <@ keypress Enter textbox

infoFooter :: DomBuilder t m => m ()
infoFooter =
  elClass "footer" "info" $ do
    el "p" (text "Double-click to edit a todo")
    el "p" $ do
      text "Template by "
      elAttr "a" ("href" =: "http://sindresorhus.com") (text "Sindre Sorhus")
    el "p" $ do
      text "Part of "
      elAttr "a" ("href" =: "http://todomvc.com") (text "TodoMVC")
