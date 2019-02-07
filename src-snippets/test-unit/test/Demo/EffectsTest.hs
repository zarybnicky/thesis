{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo.EffectsTest (tests) where

import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Trans (lift)
import Demo.BusinessLogic
import Demo.Effects
import Demo.Lib
import qualified Data.UUID.Types as UUID
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, it, shouldBe)

newtype MockAppEnv = MockAppEnv { jwtSecret :: JwtSecret }
instance Has JwtSecret MockAppEnv where
  obtain = jwtSecret

tests :: IO TestTree
tests = testSpec "EffectsTest" $
  it "should roundtrip" $ flip runReaderT (MockAppEnv (JwtSecret "")) $ do
    let payload = JwtPayload (Id UUID.nil)
    token <- mkJwtTokenImpl (Seconds 0) payload
    decoded <- decodeAndVerifyJwtTokenImpl token
    lift $ decoded `shouldBe` Just payload
