{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System.Clock.Seconds
  ( Clock(..)
  , Seconds(..)
  , getTime
  , getRes
  , fromNanoSecs
  , toNanoSecs
  , diffTimeSpec
  ) where

import Data.Coerce
import Data.Ratio
import Data.Typeable (Typeable)
import Foreign.Storable
import GHC.Generics (Generic)

import System.Clock(TimeSpec(..), Clock, s2ns, normalize)
import qualified System.Clock as C

newtype Seconds = Seconds { toTimeSpec :: TimeSpec }
 deriving (Generic, Read, Show, Typeable, Eq, Ord, Storable, Bounded)

instance Num Seconds where
  fromInteger n = Seconds $ TimeSpec (fromInteger n) 0
  Seconds (TimeSpec xs xn) * Seconds (TimeSpec ys yn) =
    Seconds $ normalize $! TimeSpec (xs*ys) (xs*yn+xn*ys+((xn*yn) `div` s2ns))
  (+) = coerce ((+) :: TimeSpec -> TimeSpec -> TimeSpec)
  (-) = coerce ((-) :: TimeSpec -> TimeSpec -> TimeSpec)
  negate = coerce (negate :: TimeSpec -> TimeSpec)
  abs = coerce (abs :: TimeSpec -> TimeSpec)
  signum (Seconds a) = case signum a of
    1 -> 1
    (-1) -> (-1)
    _ -> 0

instance Enum Seconds where
  succ x = x + 1
  pred x = x - 1
  toEnum x = Seconds $ TimeSpec (fromIntegral x) 0
  fromEnum (Seconds (TimeSpec s _)) = fromEnum s

instance Real Seconds where
  toRational (Seconds x) = toInteger x % s2ns

instance Fractional Seconds where
  fromRational x = Seconds . fromInteger $ floor (x * s2ns)
  Seconds a / Seconds b = Seconds $ a * s2ns `div` b
  recip (Seconds a) = Seconds $ s2ns * s2ns `div` a

instance RealFrac Seconds where
  properFraction (Seconds (TimeSpec s ns))
    | s >= 0 = (fromIntegral s, Seconds $ TimeSpec 0 ns)
    | otherwise = (fromIntegral (s+1), Seconds $ TimeSpec (-1) ns)

-- | The 'getTime' function shall return the current value for the
--   specified clock.
getTime :: Clock -> IO Seconds
getTime = coerce C.getTime

-- | The 'getRes' function shall return the resolution of any clock.
--   Clock resolutions are implementation-defined and cannot be set
--   by a process.
getRes :: Clock -> IO Seconds
getRes = coerce C.getRes

-- | Seconds from nano seconds.
fromNanoSecs :: Integer -> Seconds
fromNanoSecs = coerce C.fromNanoSecs

-- | Seconds to nano seconds.
toNanoSecs :: Seconds -> Integer
toNanoSecs = coerce C.toNanoSecs

-- | Compute the absolute difference.
diffTimeSpec :: Seconds -> Seconds -> Seconds
diffTimeSpec = coerce C.diffTimeSpec
