{-# LANGUAGE FlexibleContexts #-}

module Demo.Effects where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, guard)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Demo.BusinessLogic
import Demo.Lib
import qualified Web.JWT as JWT


class Monad m => MonadJwt m where
    mkJwtToken :: Seconds -> JwtPayload -> m JwtToken
    decodeAndVerifyJwtToken :: JwtToken -> m (Maybe JwtPayload)

mkJwtTokenImpl
    :: (MonadIO m, MonadReader r m, Has JwtSecret r)
    => Seconds -> JwtPayload -> m JwtToken
mkJwtTokenImpl (Seconds expiry) payload = do
    secret <- JWT.secret . unJwtSecret <$> grab
    timeNow <- liftIO getPOSIXTime
    let expiryTime = timeNow + fromIntegral expiry
    let claimsSet = JWT.def
            { JWT.exp = JWT.numericDate expiryTime
            , JWT.unregisteredClaims = jwtPayloadToMap payload
            }
    pure $ JwtToken $ JWT.encodeSigned JWT.HS256 secret claimsSet

decodeAndVerifyJwtTokenImpl
    :: (MonadIO m, MonadReader r m, Has JwtSecret r)
    => JwtToken -> m (Maybe JwtPayload)
decodeAndVerifyJwtTokenImpl (JwtToken token) = do
  secret <- JWT.secret  . unJwtSecret <$> grab
  timeNow <- JWT.numericDate <$> liftIO getPOSIXTime
  pure $ do
    claimsSet <- JWT.claims <$> JWT.decodeAndVerifySignature secret token
    expiryTimeStatedInToken <- JWT.exp claimsSet
    now <- timeNow
    guard (expiryTimeStatedInToken >= now)
    jwtPayloadFromMap $ JWT.unregisteredClaims claimsSet
