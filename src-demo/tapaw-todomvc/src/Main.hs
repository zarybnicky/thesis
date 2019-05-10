{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.FileEmbed (embedFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Storage as DOM
import qualified GHCJS.DOM.Window as DOM hiding (focus)
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import GHCJS.DOM.Types (JSString, MonadJSM, liftJSM, uncheckedCastTo)
import Language.Javascript.JSaddle.Warp (run)
import Text.Read (readMaybe)
import Reflex.Dom.Core
import System.Environment (getArgs)


data TaskFilter
  = FilterAll
  | FilterActive
  | FilterCompleted
  deriving Eq

taskFilter :: TaskFilter -> Map Int (Bool, Text) -> Map Int (Bool, Text)
taskFilter FilterAll = id
taskFilter FilterActive = M.filter (not . fst)
taskFilter FilterCompleted = M.filter fst

main :: IO ()
main = run 3000 $ do
  args <- liftIO getArgs
  liftIO $ print args
  mainWidgetWithCss $(embedFile "src/index.css") app

app :: MonadWidget t m => m ()
app = do
  window <- DOM.currentWindowUnchecked
  storage <- DOM.getLocalStorage window
  iFilter <- liftJSM $ fromMaybe FilterAll . readFilter <$> (DOM.getHash =<< DOM.getLocation window)
  eFilter <- wrapDomEvent window (`DOM.on` DOM.hashChange)
    (DOM.getHash =<< DOM.getLocation window)
  iTasks <- readTasks storage

  elClass "section" "todoapp" $ do
    eNewItem <- fmap (False,) <$> inputBox
    dFilter <- holdDyn iFilter (fromMaybe FilterAll . readFilter <$> eFilter)
    rec
      let bNextIdx = (+ 1) . M.foldlWithKey (\a b _ -> max a b) (-1) <$> current dTasks
      dTasks <- foldDyn ($) iTasks $ leftmost
        [ M.insert <$> bNextIdx <@> eNewItem
        , toggleAll <$ eToggleAll
        , (\(k, mv) -> M.update (const mv) k) <$> eEdit
        , M.filter (not . fst) <$ eClearCompleted
        ]
      (eToggleAll, eEdit) <- taskList (taskFilter <$> dFilter <*> dTasks)
      eClearCompleted <- navigation dFilter dTasks
    performEvent_ $ saveTasks storage <$> traceEvent "tasks" (updated dTasks)
  infoFooter

readFilter :: Text -> Maybe TaskFilter
readFilter "" = Just FilterAll
readFilter "#" = Just FilterAll
readFilter "#/all" = Just FilterAll
readFilter "#/active" = Just FilterActive
readFilter "#/completed" = Just FilterCompleted
readFilter _ = Nothing

saveTasks :: MonadJSM m => DOM.Storage -> Map Int (Bool, Text) -> m ()
saveTasks st m = DOM.setItem st ("todosReflex" :: JSString) (show $ M.toList m)

readTasks :: MonadJSM m => DOM.Storage -> m (Map Int (Bool, Text))
readTasks st = maybe M.empty M.fromList . (readMaybe =<<) <$> DOM.getItem st ("todosReflex" :: JSString)

toggleAll :: Map Int (Bool, Text) -> Map Int (Bool, Text)
toggleAll m =
  if not (all fst m)
    then M.map (_1 .~ True) m
    else M.map (_1 .~ False) m

navigation ::
     MonadWidget t m
  => Dynamic t TaskFilter
  -> Dynamic t (Map Int (Bool, Text))
  -> m (Event t ())
navigation dFilter dTasks =
  elDynAttr "footer"
      (("class" =: "footer" <>) . bool ("style" =: "display:none") mempty . (0 <) . M.size <$> dTasks) $ do
    elClass "span" "todo-count" $ do
      el "strong" (display dNumCompleted)
      dynText $ bool " items left" " item left" . (== 1) <$> dNumCompleted
    elClass "ul" "filters" $ do
      el "li" $ elDynAttr "a"
        (("href" =: "#/" <>) . bool mempty ("class" =: "selected") . (== FilterAll) <$> dFilter)
        (text "All")
      el "li" $ elDynAttr "a"
        (("href" =: "#/active" <>) . bool mempty ("class" =: "selected") . (== FilterActive) <$> dFilter)
        (text "Active")
      el "li" $ elDynAttr "a"
        (("href" =: "#/completed" <>) . bool mempty ("class" =: "selected") . (== FilterCompleted) <$> dFilter)
        (text "Completed")

    (btn, ()) <- elDynAttr' "button"
      (("class" =: "clear-completed" <>) . bool ("style" =: "display:none") mempty . (0 <) <$> dNumCompleted)
      (text "Clear completed")
    pure (domEvent Click btn)
  where
    dNumCompleted = length . filter fst . M.elems <$> dTasks

taskList ::
     MonadWidget t m
  => Dynamic t (Map Int (Bool, Text))
  -> m (Event t (), Event t (Int, Maybe (Bool, Text)))
taskList dTasks =
  elClass "section" "main" $ do
    let dAllDone = all fst . M.elems <$> dTasks
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
     MonadWidget t m
  => Int
  -> Dynamic t (Bool, Text)
  -> Dynamic t Bool
  -> m (Event t (Maybe (Bool, Text)))
taskItem _ dValue _ = do
  let (dChecked, dText) = splitDynPure dValue
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
    [ (Just .) . (,) <$> current dChecked <@> eUpdated
    , (Just .) . flip (,) <$> current dText <@> eToggle
    , Nothing <$ eDestroy
    ]

taskView ::
     MonadWidget t m
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

taskEdit :: MonadWidget t m => Dynamic t Text -> Event t () -> m (Event t Text)
taskEdit dText eToggle = do
  rec
    textbox <- inputElement $ def
      & inputElementConfig_setValue .~ (current dText <@ eToggle <> keydown Escape textbox)
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "edit"
  let textbox' = _element_raw $ _inputElement_element textbox
  eDelayed <- delay 0.05 eToggle
  performEvent_ $ DOM.focus (uncheckedCastTo DOM.HTMLElement textbox') <$ eDelayed
  pure $ leftmost
    [ current (value textbox) <@ keypress Enter textbox <> domEvent Blur textbox
    , current dText <@ keydown Escape textbox
    ]

inputBox :: MonadWidget t m => m (Event t Text)
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

infoFooter :: MonadWidget t m => m ()
infoFooter =
  elClass "footer" "info" $ do
    el "p" (text "Double-click to edit a todo")
    el "p" $ do
      text "Template by "
      elAttr "a" ("href" =: "http://sindresorhus.com") (text "Sindre Sorhus")
    el "p" $ do
      text "Part of "
      elAttr "a" ("href" =: "http://todomvc.com") (text "TodoMVC")
