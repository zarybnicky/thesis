{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant
import System.IO (hPutStrLn, stderr)


data Item = Item
  { itemId :: Integer
  , itemText :: String
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item


main :: IO ()
main = do
  let port = 3000 :: Int
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings (serve (Proxy @ItemApi) server)

server :: Server ItemApi
server = getItems :<|> getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById =
  \case
    0 -> return exampleItem
    _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"
