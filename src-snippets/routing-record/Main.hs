module Main
  ( main
  ) where

import Language.Javascript.JSaddle.Warp as JS (run)
import Reflex.Dom.Core hiding (Link, Widget)
import Servant.App (app)

main :: IO ()
main = run 3000 $ mainWidget app



-- app :: forall t m. MonadWidget t m => m ()
-- app = void $ (void $ serve api localhost widgets errorPageLoc errorPage)
--   where localhost = pure (Authority Http "localhost" (Just 8080) "")

-- data Item = Item
--   { itemId :: Int
--   , itemName :: String
--   , itemPrice :: Double
--   } deriving (Show, Generic, FromJSON, ToJSON)

-- type API = "item" :> "all" :> Get '[JSON,RunTime] [Item]
--       :<|> "item" :> "one" :> Capture "itemId" Int :> Get '[JSON,RunTime] Item
--       :<|> "item" :> "num" :> Reflexive (Get '[JSON] Int)
--       :<|> Get '[JSON,RunTime] ()

-- api :: Proxy API
-- api = Proxy

-- item :: MonadWidget t m => Item -> m ()
-- item (Item i name p) =
--   el "div" $ do
--     el "div" $ text $ "Item: "  <> T.pack name
--     el "div" $ text $ "Id: "    <> T.pack (show i)
--     el "div" $ text $ "Price: " <> T.pack (show p)

-- widgets :: MonadWidget t m => Links API t m -> Widgets API t m
-- widgets (jumpAll :<|> jumpOne :<|> eLens :<|> jumpHome) =
--   displayAll jumpOne jumpHome :<|> displayOne jumpAll jumpHome :<|> () :<|> displayHome eLens jumpAll
--   where displayAll jumpOne jumpHome items = Link $ do
--           mapM_ (el "div" . item) items
--           el "div" $ text "Jump to an item based off of its id: "
--           t <- textInput $ def & textInputConfig_inputType .~ "number"
--                                & textInputConfig_initialValue .~ "0"
--           unLink $ jumpOne (fmap (maybe 0 id . readMaybe . T.unpack) (_textInput_value t))
--                            (fmapMaybe (\x -> if keyCodeLookup x == Enter then Just () else Nothing)
--                                       (_textInput_keypress t))
--         displayOne jumpAll jumpHome i = Link $ do
--           item i
--           h <- button "Jump home!"
--           unLink (jumpHome h)
--         displayHome eLens jumpAll () = Link $ do
--           n <- button "Refresh length"
--           len <- fmapMaybe hush <$> eLens n
--           el "div" $ do
--             text "Number of items:"
--             holdDyn 0 len >>= display
--           a <- button "Jump All!"
--           unLink (jumpAll a)
--         hush (Left _)  = Nothing
--         hush (Right a) = Just a

-- errorPage :: (Monad m, DomBuilder t m) => Links API t m -> ServantErr -> Link t m
-- errorPage (_ :<|> _ :<|> _ :<|> jumpHome) err = Link $ do
--   el "div" $ text $ "Something went wrong : " <> displayErr err
--   b <- button "Jump home!"
--   unLink (jumpHome b)
--   where displayErr (NotFound err) = "Not Found: " <> err
--         displayErr (AjaxFailure err) = "Ajax failure: " <> err

-- errorPageLoc :: Uri
-- errorPageLoc = Uri ["error"] []
