{-# LANGUAGE TypeApplications #-}

module Demo.BusinessLogicTest (tests) where

import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.Word (Word32)
import Demo.BusinessLogic
import Hedgehog (MonadGen, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "BusinessLogic"
  [ testProperty "jwtMapEncodeAndDecode" $ property $ do
      randomId <- forAll genRandId
      let randomJwtPayload = JwtPayload { jwtUserId = Id randomId }
      let encoded = jwtPayloadToMap randomJwtPayload
      let decoded = jwtPayloadFromMap encoded
      decoded === Just randomJwtPayload
  ]

genRandId :: MonadGen m => m UUID
genRandId = UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 (Range.constantBounded @Word32)
