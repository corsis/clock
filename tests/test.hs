import Test.Tasty
import Test.Tasty.QuickCheck as QuickCheck
import Data.Fixed
import Data.List
-- import Test.Tasty.HUnit as HUnit
import System.Clock
import System.Clock.Seconds as S

instance Arbitrary TimeSpec where
  arbitrary = do
    sec <- arbitrarySizedIntegral
    nan <- arbitrarySizedIntegral
    return $ TimeSpec sec nan

deriving instance Arbitrary Seconds

main = defaultMain (localOption (QuickCheckTests 100000) $ tests)

tests :: TestTree
tests  = testGroup "All tests" [timeSpecTests, secondsTests]

timeSpecTests = testGroup "TimeSpec tests" [qcNumInstance (0 :: TimeSpec), qcRealInstance (0 :: TimeSpec), qcTimeSpec]
secondsTests = testGroup "Seconds tests" [qcNumInstance (0 :: S.Seconds), qcRealInstance (0 :: S.Seconds), qcSeconds]

qcNumInstance :: (Eq a, Num a, Arbitrary a, Show a) => a -> TestTree
qcNumInstance (s :: a) = testGroup "Num"
  [
    QuickCheck.testProperty "Associativity of (+)" $ \(x :: a) y z ->
        (x + y) + z == x + (y + z)
  , QuickCheck.testProperty "Commutativity of (+)" $ \(x :: a) y ->
        x + y == y + x
  , QuickCheck.testProperty "fromInteger 0 is the additive identity" $ \(x :: a) ->
        x + fromInteger 0 == x
  , QuickCheck.testProperty "negate gives the additive inverse" $ \(x :: a) ->
        x + negate x == fromInteger 0
  , QuickCheck.testProperty "fromInteger 1 is the multiplicative identity" $ \(x :: a) ->
        x * fromInteger 1 == x && fromInteger 1 * x == x
  , QuickCheck.testProperty "neg(neg(x)) = x" $ \(x :: a) ->
        negate (negate x) == x
  , QuickCheck.testProperty "x = abs(x) * signum(x)" $ \(x :: a) ->
        x == (abs x) * (signum x)
  ]

qcRealInstance :: (Real a, Arbitrary a, Show a) => a -> TestTree
qcRealInstance (s :: a) = testGroup "Real"
  [
    QuickCheck.testProperty "integer addition is correct" $ \ x y ->
      toRational (x + y) == toRational (fromInteger x + fromInteger y :: a)
  , QuickCheck.testProperty "integer subtraction is correct" $ \ x y ->
      toRational (x - y) == toRational (fromInteger x - fromInteger y :: a)
  , QuickCheck.testProperty "integer multiplication is correct" $ \ x y ->
      toRational (x * y) == toRational (fromInteger x * fromInteger y :: a)
  , QuickCheck.testProperty "random list of TimeSpecs is sorted like equivalent list of rationals" $ \(x :: [a]) ->
      map toRational (sort x) == sort (map toRational x)
  ]

qcTimeSpec :: TestTree
qcTimeSpec = testGroup "TimeSpec-specific"
  [
    -- fails with Seconds on 0.000000001 * -1.000000002 * -2.000000001
    QuickCheck.testProperty "Associativity of (*)" $ \(x :: TimeSpec) y z ->
        (x * y) * z == x * (y * z)
    -- fails with Seconds on [-0.999999999,0.000000001,-1.000000001]
  , QuickCheck.testProperty "Distributivity of (*) with respect to (+)" $ \(a :: TimeSpec) b c ->
        a * (b + c) == (a * b) + (a * c) && (b + c) * a == (b * a) + (c * a)
  , QuickCheck.testProperty "TimeSpec Quot-rem division equality" $ \(x :: TimeSpec) y ->
      y == 0 || x == y * quot x y + rem x y
  , QuickCheck.testProperty "TimeSpec Rem is within bounds" $ \(x :: TimeSpec) y ->
      let r = rem x y in y == 0 || r == fromInteger 0 || abs r < abs y
  , QuickCheck.testProperty "TimeSpec quotRem agrees with quot and rem" $ \(x :: TimeSpec) y ->
      let (q,r) = quotRem x y in
        y == 0 || (q == quot x y && r == rem x y)
  , QuickCheck.testProperty "TimeSpec Div-mod division equality" $ \(x :: TimeSpec) y ->
      y == 0 || x == y * div x y + mod x y
  , QuickCheck.testProperty "TimeSpec Mod is within bounds" $ \(x :: TimeSpec) y ->
      let r = mod x y in y == 0 || (r == fromInteger 0 || abs r < abs y)
  , QuickCheck.testProperty "TimeSpec divMod agrees with div and mod" $ \(x :: TimeSpec) y ->
      let (q,r) = divMod x y in
        y == 0 || (q == div x y && r == mod x y)
  , QuickCheck.testProperty "TimeSpec toInteger . fromInteger is the identity" $ \x ->
      x == toInteger (fromInteger x :: TimeSpec)
  , QuickCheck.testProperty "TimeSpec fromInteger . toInteger is the identity" $ \(x :: TimeSpec) ->
      x == fromInteger (toInteger x)
  , QuickCheck.testProperty "TimeSpec division agrees with Integer" $ \(x :: TimeSpec) y ->
      y == 0 || toInteger (x `div` y) == toInteger x `div` toInteger y
  , QuickCheck.testProperty "TimeSpec quot agrees with Integer" $ \(x :: TimeSpec) y ->
      y == 0 || toInteger (x `quot` y) == toInteger x `quot` toInteger y
  ]

qcSeconds :: TestTree
qcSeconds = testGroup "Seconds-specific"
  [
    QuickCheck.testProperty "Seconds multiplication is Nano multiplication" $ \x y ->
      let nano = toRational $ (x :: Nano) * (y :: Nano)
          seconds = toRational $ (realToFrac x) * (realToFrac y :: Seconds)
      in  nano == seconds
  , QuickCheck.testProperty "Seconds truncate is Nano truncate" $ \(x :: Nano) ->
      let nano = truncate x :: Integer
          seconds = truncate (realToFrac x :: Seconds)
      in  nano == seconds
  , QuickCheck.testProperty "Seconds / is Nano /" $ \(x :: Nano) (y :: Nano) ->
      let nano = toRational $ x / y
          seconds = toRational (realToFrac x / realToFrac y :: Seconds)
      in y == 0 || nano == seconds
  , QuickCheck.testProperty "Seconds recip is Nano recip" $ \(x :: Nano) ->
      let nano = toRational $ recip x
          seconds = toRational (recip $ realToFrac x :: Seconds)
      in x == 0 || nano == seconds
  ]
