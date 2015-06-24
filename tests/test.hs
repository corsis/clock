import Test.Tasty
import Test.Tasty.QuickCheck as QuickCheck
-- import Test.Tasty.HUnit as HUnit
import System.Clock

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [quickCheckTests]

quickCheckTests = testGroup "QuickCheck tests" [qcNumClass]

instance Arbitrary TimeSpec where
  arbitrary = do
    sec <- arbitrarySizedIntegral
    nan <- arbitrarySizedIntegral
    return $ TimeSpec sec nan

qcNumClass = testGroup "quick checking"
  [ 
    QuickCheck.testProperty "x = abs(x) * signum(x)" $
      \ x -> (x :: TimeSpec) == (abs x) * (signum x)
  , QuickCheck.testProperty "integer addition equals TimeSpec addition" $
      \ x y -> x + y == timeSpecAsNanoSecs (fromInteger x + fromInteger y)
  , QuickCheck.testProperty "rational multiplication equals TimeSpec multiplication" $
      \ x y ->
        let
          rationalMul = truncate ((x :: Rational) * (y :: Rational) * (10^9))
	  timespecMul = timeSpecAsNanoSecs (
              fromInteger (truncate (x * 10^9))
            * fromInteger (truncate (y * 10^9)))
	in
          -- there is some rounding error, we cannot compare directly with ==
	  abs (rationalMul - timespecMul) <= 10
  ]
