import Test.Tasty

import qualified Demo.EffectsTest as Effects
import qualified Demo.BusinessLogicTest as BusinessLogic
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unsafePerformIO Effects.tests
  , BusinessLogic.tests
  ]
