import Test.Tasty
import Test.Tasty.QuickCheck as QuickCheck

import System.Clock

main = defaultMain tests

tests :: TestTree
tests = testGroup "Try" [qcAbsSignum]

instance Arbitrary TimeSpec where
  arbitrary = do
    sec <- arbitrarySizedIntegral
    nan <- arbitrarySizedIntegral
    return $ TimeSpec sec nan

qcAbsSignum = testGroup "quick checking"
  [ 
    QuickCheck.testProperty "x = abs(x) * signum (x)" $ 
      \x -> (x :: TimeSpec) == (abs x) * (signum x)
  ]
