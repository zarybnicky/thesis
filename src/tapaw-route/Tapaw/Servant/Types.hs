module Tapaw.Servant.Types
  ( Loc(..)
  , Err(..)
  , App
  ) where

import Data.Text (Text)
import URI.ByteString (URIParseError)

data Loc = Loc
  { locPath :: [Text]
  , locQuery :: [(Text, Text)]
  } deriving (Show)

instance Semigroup Loc where
  a <> _ = a

data Err
  = Err400 Text
  | Err401
  | Err404
  | Err500 URIParseError
  | Err501
  deriving (Eq, Show)

data App
