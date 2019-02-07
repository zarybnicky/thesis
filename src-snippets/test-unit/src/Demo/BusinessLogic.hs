{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Demo.BusinessLogic where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(String))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.UUID.Types as UUID
import GHC.Generics (Generic)
import System.Random (newStdGen, randomRs)



newtype Id a = Id { unId :: UUID.UUID }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord)

type AnyId = Id ()

castId :: forall to from . Id from -> Id to
castId (Id a) = Id a



newtype JwtSecret = JwtSecret { unJwtSecret :: Text }

newtype JwtToken = JwtToken { unJwtToken :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord)

newtype JwtPayload = JwtPayload
    { jwtUserId :: AnyId
    } deriving (Eq, Show)


-- | Makes a random string comprised of a - z of a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = pack . take len . randomRs ('a', 'z') <$> liftIO newStdGen


jwtPayloadToMap :: JwtPayload -> Map Text Value
jwtPayloadToMap JwtPayload{..} = Map.fromList [("id", String $ UUID.toText $ unId jwtUserId)]

jwtPayloadFromMap :: Map Text Value -> Maybe JwtPayload
jwtPayloadFromMap claimsMap = do
    String jwtId <- Map.lookup "id" claimsMap
    jwtUserId <- Id <$> UUID.fromText jwtId
    pure JwtPayload{..}
