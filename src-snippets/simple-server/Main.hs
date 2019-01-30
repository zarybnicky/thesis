{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (Text, pack)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant
import Servant.Swagger (toSwagger)
import System.IO (hPutStrLn, stderr)


type Api =
  Get '[PlainText] Text :<|>
  "hello" :> Capture "name" Text :> Get '[JSON] [Text]


main :: IO ()
main = do
  let port = 3000 :: Int
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings (serve (Proxy @Api) server)


server :: Server Api
server = swagger :<|> hello

swagger :: Handler Text
swagger = pure . pack . unpack . encodePretty . toSwagger $ Proxy @Api

hello :: Text -> Handler [Text]
hello x = pure ["Hello", x, "!"]
