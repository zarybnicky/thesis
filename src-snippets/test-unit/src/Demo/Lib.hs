{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Demo.Lib where

import Control.Monad.Reader (MonadReader, asks)


class Has field env where
    obtain :: env -> field

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field



newtype Seconds = Seconds { unSeconds :: Int } deriving (Show, Eq)
