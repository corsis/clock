-- | High-resolution, realtime clock and timer functions for Posix
--   systems. This module is being developed according to IEEE Std
--   1003.1-2008: <http://www.opengroup.org/onlinepubs/9699919799/>,
--   <http://www.opengroup.org/onlinepubs/9699919799/functions/clock_getres.html#>

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- To allow importing Data.Int and Data.Word indiscriminately on all platforms,
-- since we can't systematically predict what typedef's expand to.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module System.Clock
  ( Clock(..)
  , TimeSpec(..)
  , getTime
  , getRes
  , fromNanoSecs
  , toNanoSecs
  , diffTimeSpec
  , timeSpecAsNanoSecs
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Int
import Data.Word
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import GHC.Generics (Generic)

#if defined(_WIN32)
#  include "hs_clock_win32.c"
#elif defined(__MACH__) && defined(__APPLE__)
#  include "hs_clock_darwin.c"
#else
#  include <time.h>
-- Due to missing define in FreeBSD 9.0 and 9.1
-- (http://lists.freebsd.org/pipermail/freebsd-stable/2013-September/075095.html).
#  ifndef CLOCK_PROCESS_CPUTIME_ID
#    define CLOCK_PROCESS_CPUTIME_ID 15
#  endif
#endif

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Clock types. A clock may be system-wide (that is, visible to all processes)
--   or per-process (measuring time that is meaningful only within a process).
--   All implementations shall support CLOCK_REALTIME. (The only suspend-aware
--   monotonic is CLOCK_BOOTTIME on Linux.)
data Clock

    -- | The identifier for the system-wide monotonic clock, which is defined as
    --   a clock measuring real time, whose value cannot be set via
    --   @clock_settime@ and which cannot have negative clock jumps. The maximum
    --   possible clock jump shall be implementation defined. For this clock,
    --   the value returned by 'getTime' represents the amount of time (in
    --   seconds and nanoseconds) since an unspecified point in the past (for
    --   example, system start-up time, or the Epoch). This point does not
    --   change after system start-up time. Note that the absolute value of the
    --   monotonic clock is meaningless (because its origin is arbitrary), and
    --   thus there is no need to set it. Furthermore, realtime applications can
    --   rely on the fact that the value of this clock is never set.
  = Monotonic

    -- | The identifier of the system-wide clock measuring real time. For this
    --   clock, the value returned by 'getTime' represents the amount of time (in
    --   seconds and nanoseconds) since the Epoch.
  | Realtime

    -- | The identifier of the CPU-time clock associated with the calling
    --   process. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current process.
  | ProcessCPUTime

    -- | The identifier of the CPU-time clock associated with the calling OS
    --   thread. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current OS thread.
  | ThreadCPUTime

#if defined (linux_HOST_OS)

    -- | (since Linux 2.6.28; Linux-specific)
    --   Similar to CLOCK_MONOTONIC, but provides access to a
    --   raw hardware-based time that is not subject to NTP
    --   adjustments or the incremental adjustments performed by
    --   adjtime(3).
  | MonotonicRaw

    -- | (since Linux 2.6.39; Linux-specific)
    --   Identical to CLOCK_MONOTONIC, except it also includes
    --   any time that the system is suspended.  This allows
    --   applications to get a suspend-aware monotonic clock
    --   without having to deal with the complications of
    --   CLOCK_REALTIME, which may have discontinuities if the
    --   time is changed using settimeofday(2).
  | Boottime

    -- | (since Linux 2.6.32; Linux-specific)
    --   A faster but less precise version of CLOCK_MONOTONIC.
    --   Use when you need very fast, but not fine-grained timestamps.
  | MonotonicCoarse

    -- | (since Linux 2.6.32; Linux-specific)
    --   A faster but less precise version of CLOCK_REALTIME.
    --   Use when you need very fast, but not fine-grained timestamps.
  | RealtimeCoarse

#endif

  deriving (Eq, Enum, Generic, Read, Show, Typeable)

#if defined(_WIN32)
foreign import ccall hs_clock_win32_gettime_monotonic :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_gettime_realtime :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_gettime_processtime :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_gettime_threadtime :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_getres_monotonic :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_getres_realtime :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_getres_processtime :: Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_win32_getres_threadtime :: Ptr TimeSpec -> IO ()
#elif defined(__MACH__) && defined(__APPLE__)
foreign import ccall hs_clock_darwin_gettime :: #{type clock_id_t} -> Ptr TimeSpec -> IO ()
foreign import ccall hs_clock_darwin_getres  :: #{type clock_id_t} -> Ptr TimeSpec -> IO ()
#else
foreign import ccall clock_gettime :: #{type clockid_t} -> Ptr TimeSpec -> IO ()
foreign import ccall clock_getres  :: #{type clockid_t} -> Ptr TimeSpec -> IO ()
#endif

#if defined(_WIN32)
#elif defined(__MACH__) && defined(__APPLE__)
clockToConst :: Clock -> #{type clock_id_t}
clockToConst Monotonic = #const SYSTEM_CLOCK
clockToConst  Realtime = #const CALENDAR_CLOCK
clockToConst ProcessCPUTime = #const SYSTEM_CLOCK
clockToConst  ThreadCPUTime = #const SYSTEM_CLOCK
#else
clockToConst :: Clock -> #{type clockid_t}
clockToConst Monotonic = #const CLOCK_MONOTONIC
clockToConst  Realtime = #const CLOCK_REALTIME
clockToConst ProcessCPUTime = #const CLOCK_PROCESS_CPUTIME_ID
clockToConst  ThreadCPUTime = #const CLOCK_THREAD_CPUTIME_ID
#if defined (linux_HOST_OS)
clockToConst    MonotonicRaw = #const CLOCK_MONOTONIC_RAW
#if defined(CLOCK_BOOTTIME)
clockToConst        Boottime = #const CLOCK_BOOTTIME
#else
clockToConst        Boottime = 7
#endif
clockToConst MonotonicCoarse = #const CLOCK_MONOTONIC_COARSE
clockToConst  RealtimeCoarse = #const CLOCK_REALTIME_COARSE
#endif
#endif

allocaAndPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaAndPeek f = alloca $ \ptr -> f ptr >> peek ptr

-- | The 'getTime' function shall return the current value for the
--   specified clock.
getTime :: Clock -> IO TimeSpec

-- | The 'getRes' function shall return the resolution of any clock.
--   Clock resolutions are implementation-defined and cannot be set
--   by a process.
getRes :: Clock -> IO TimeSpec

#if defined(_WIN32)
getTime Monotonic = allocaAndPeek hs_clock_win32_gettime_monotonic
getTime Realtime = allocaAndPeek hs_clock_win32_gettime_realtime
getTime ProcessCPUTime = allocaAndPeek hs_clock_win32_gettime_processtime
getTime ThreadCPUTime = allocaAndPeek hs_clock_win32_gettime_threadtime
#elif defined(__MACH__) && defined(__APPLE__)
getTime clk = allocaAndPeek $! hs_clock_darwin_gettime $! clockToConst clk
#else
getTime clk = allocaAndPeek $! clock_gettime $! clockToConst clk
#endif

#if defined(_WIN32)
getRes Monotonic = allocaAndPeek hs_clock_win32_getres_monotonic
getRes Realtime = allocaAndPeek hs_clock_win32_getres_realtime
getRes ProcessCPUTime = allocaAndPeek hs_clock_win32_getres_processtime
getRes ThreadCPUTime = allocaAndPeek hs_clock_win32_getres_threadtime
#elif defined(__MACH__) && defined(__APPLE__)
getRes clk = allocaAndPeek $! hs_clock_darwin_getres $! clockToConst clk
#else
getRes clk = allocaAndPeek $! clock_getres $! clockToConst clk
#endif

-- | TimeSpec structure
data TimeSpec = TimeSpec
  { sec  :: {-# UNPACK #-} !Int64 -- ^ seconds
  , nsec :: {-# UNPACK #-} !Int64 -- ^ nanoseconds
  } deriving (Generic, Read, Show, Typeable)

#if defined(_WIN32)
instance Storable TimeSpec where
  sizeOf _ = sizeOf (undefined :: Int64) * 2
  alignment _ = alignment (undefined :: Int64)
  poke ptr ts = do
      pokeByteOff ptr 0 (sec ts)
      pokeByteOff ptr (sizeOf (undefined :: Int64)) (nsec ts)
  peek ptr = do
      TimeSpec
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr (sizeOf (undefined :: Int64))
#else
instance Storable TimeSpec where
  sizeOf _ = #{size struct timespec}
  alignment _ = #{alignment struct timespec}
  poke ptr ts = do
      let xs :: #{type time_t} = fromIntegral $ sec ts
          xn :: #{type long} = fromIntegral $ nsec ts
      #{poke struct timespec, tv_sec} ptr (xs)
      #{poke struct timespec, tv_nsec} ptr (xn)
  peek ptr = do
      xs :: #{type time_t} <- #{peek struct timespec, tv_sec} ptr
      xn :: #{type long} <- #{peek struct timespec, tv_nsec} ptr
      return $ TimeSpec (fromIntegral xs) (fromIntegral xn)
#endif

s2ns :: Num a => a
s2ns = 10^9

normalize :: TimeSpec -> TimeSpec
normalize (TimeSpec xs xn) | xn < 0 || xn >= s2ns = TimeSpec (xs + q)  r
                           | otherwise            = TimeSpec  xs      xn
                             where (q, r) = xn `divMod` s2ns

instance Num TimeSpec where
  (TimeSpec xs xn) + (TimeSpec ys yn) = normalize $! TimeSpec (xs + ys) (xn + yn)
  (TimeSpec xs xn) - (TimeSpec ys yn) = normalize $! TimeSpec (xs - ys) (xn - yn)
  (TimeSpec xs xn) * (TimeSpec ys yn) = normalize $! TimeSpec (xsi_ysi) (xni_yni)
                                         where xsi_ysi = fromInteger $!  xsi*ysi 
                                               xni_yni = fromInteger $! (xni*yni + (xni*ysi + xsi*yni) * s2ns) `div` s2ns
                                               xsi     =   toInteger  xs
                                               ysi     =   toInteger  ys
                                               xni     =   toInteger  xn
                                               yni     =   toInteger  yn
--     let xsi = toInteger xs    -- convert to arbitraty Integer type to avoid int overflow
--         xni = toInteger xn
--         ysi = toInteger ys
--           yni = toInteger yn    -- seconds                 -- nanoseconds
--       in normalize $! TimeSpec (fromInteger $! xsi * ysi) (fromInteger $! (xni * yni + (xni * ysi + xsi * yni) * s2ns) `div` s2ns)

  negate (TimeSpec xs xn) = normalize $! TimeSpec (negate xs) (negate xn)
  abs    (normalize -> TimeSpec xs xn) | xs == 0   = normalize $! TimeSpec 0 xn
                                       | otherwise = normalize $! TimeSpec (abs xs) (signum xs * xn)
  signum (normalize -> TimeSpec xs xn) | xs == 0   = TimeSpec (signum xn) 0
                                       | otherwise = TimeSpec (signum xs) 0
  fromInteger x = TimeSpec (fromInteger q) (fromInteger r) where (q, r) = x `divMod` s2ns

instance Eq TimeSpec where
  (normalize -> TimeSpec xs xn) == (normalize -> TimeSpec ys yn) | True == es = xn == yn
                                                                 | otherwise  = es
                                                                   where   es = xs == ys

instance Ord TimeSpec where
  compare (normalize -> TimeSpec xs xn) (normalize -> TimeSpec ys yn) | EQ ==  os = compare xn yn
                                                                      | otherwise = os
                                                                        where  os = compare xs ys

-- | TimeSpec from nano seconds.
fromNanoSecs :: Integer -> TimeSpec
fromNanoSecs x = TimeSpec (fromInteger  q) (fromInteger  r) where (q, r) = x `divMod` s2ns


-- | TimeSpec to nano seconds.
toNanoSecs :: TimeSpec -> Integer
toNanoSecs   (TimeSpec  (toInteger -> s) (toInteger -> n)) = s * s2ns + n

-- | Compute the absolute difference.
diffTimeSpec :: TimeSpec -> TimeSpec -> TimeSpec
diffTimeSpec ts1 ts2 = abs (ts1 - ts2)

{-# DEPRECATED timeSpecAsNanoSecs "Use toNanoSecs instead! Replaced timeSpecAsNanoSecs with the same signature TimeSpec -> Integer" #-}
-- | TimeSpec as nano seconds.
timeSpecAsNanoSecs :: TimeSpec -> Integer
timeSpecAsNanoSecs   (TimeSpec s n) = toInteger s * s2ns + toInteger n
