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

-- data Context contextTypes where
--     EmptyContext :: Context '[]
--     (:.) :: x -> Context xs -> Context (x ': xs)
-- infixr 5 :.

-- class HasContextEntry (context :: [*]) (val :: *) where
--   getContextEntry :: Context context -> val

-- instance {-# OVERLAPPABLE #-} HasContextEntry xs val => HasContextEntry (notIt ': xs) val where
--   getContextEntry (_ :. xs) = getContextEntry xs

-- instance {-# OVERLAPPING #-} HasContextEntry (val ': xs) val where
--   getContextEntry (x :. _) = x
