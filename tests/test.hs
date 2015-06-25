import Test.Tasty
import Test.Tasty.QuickCheck as QuickCheck
import Data.Fixed
import Data.List
-- import Test.Tasty.HUnit as HUnit
import System.Clock

instance Arbitrary TimeSpec where
  arbitrary = do
    sec <- arbitrarySizedIntegral
    nan <- arbitrarySizedIntegral
    return $ TimeSpec sec nan

main = defaultMain adjustOption (QuickCheckTests 100000 +) $ tests

tests :: TestTree
tests = testGroup "All tests" [numClassTests, eqOrdClassesTests]

numClassTests = testGroup "Num class tests" [
  -- let's make at least 100,000 tests
  qcNumClass
  ]

eqOrdClassesTests = testGroup "Eq and Ord instance tests" [
  -- let's make at least 100,000 tests
  qcEqOrdClass
  ]

qcNumClass = testGroup "QuickCheck"
  [ 
    QuickCheck.testProperty "x = abs(x) * signum(x)" $
      \ x -> (x :: TimeSpec) == (abs x) * (signum x)
  , QuickCheck.testProperty "integer addition equals TimeSpec addition" $
      \ x y -> x + y == timeSpecAsNanoSecs (fromInteger x + fromInteger y)
  , QuickCheck.testProperty "integer substraction equals TimeSpec addition" $
      \ x y -> x - y == timeSpecAsNanoSecs (fromInteger x - fromInteger y)
  , QuickCheck.testProperty
      "rational multiplication equals TimeSpec multiplication" $
      \ x y ->
        let
          rationalMul = truncate ((x :: Nano) * (y :: Nano) * (10^9))
          timespecMul = timeSpecAsNanoSecs (
              fromInteger (truncate (x * 10^9))
            * fromInteger (truncate (y * 10^9)))
        in
          rationalMul == timespecMul
  , QuickCheck.testProperty "neg(neg(x)) = x" $
      \ x -> negate (negate x :: TimeSpec) == x
  ]

qcEqOrdClass = testGroup "QuickCheck"
  [
    QuickCheck.testProperty
      "random list of TimeSpecs is sorted like equivalent list of integers" $
      \ x -> sort (x :: [TimeSpec]) == map (fromInteger) (sort (map timeSpecAsNanoSecs x))
  ]
