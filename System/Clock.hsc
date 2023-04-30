-- | High-resolution, realtime clock and timer functions for Posix
--   systems. This module is being developed according to IEEE Std
--   1003.1-2008: <http://www.opengroup.org/onlinepubs/9699919799/>,
--   <http://www.opengroup.org/onlinepubs/9699919799/functions/clock_getres.html#>

{-# LANGUAGE CApiFFI #-}
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
  , normalize
  , s2ns
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Int
import Data.Word
import Data.Ratio
import Data.Typeable (Typeable)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import GHC.Generics (Generic)

#if defined(_WIN32)
#  include "hs_clock_win32.c"
#  define HS_CLOCK_HAVE_PROCESS_CPUTIME
#  define HS_CLOCK_HAVE_THREAD_CPUTIME
#else
#  include <time.h>
#  ifdef CLOCK_PROCESS_CPUTIME_ID
#    define HS_CLOCK_HAVE_PROCESS_CPUTIME
#  endif
#  ifdef CLOCK_THREAD_CPUTIME_ID
#    define HS_CLOCK_HAVE_THREAD_CPUTIME
#  endif
import System.Posix.Types
#endif

#if __GLASGOW_HASKELL__ < 800
#  let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | Clock types. A clock may be system-wide (that is, visible to all processes)
--   or per-process (measuring time that is meaningful only within a process).
--   All implementations shall support 'Realtime'.
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
    --   (Identical to 'Boottime' since Linux 4.17, see https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=d6ed449afdb38f89a7b38ec50e367559e1b8f71f)
    --  @CLOCK_MONOTONIC@ (macOS - @SYSTEM_CLOCK@)
  = Monotonic

    -- | The identifier of the system-wide clock measuring real time. For this
    --   clock, the value returned by 'getTime' represents the amount of time (in
    --   seconds and nanoseconds) since the Epoch.
    -- @CLOCK_REALTIME@ (macOS - @CALENDAR_CLOCK@, Windows - @GetSystemTimeAsFileTime@)
  | Realtime

#ifdef HS_CLOCK_HAVE_PROCESS_CPUTIME
    -- | The identifier of the CPU-time clock associated with the calling
    --   process. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current process.
  | ProcessCPUTime
#endif

#ifdef HS_CLOCK_HAVE_THREAD_CPUTIME
    -- | The identifier of the CPU-time clock associated with the calling OS
    --   thread. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current OS thread.
  | ThreadCPUTime
#endif

#if defined (CLOCK_MONOTONIC_RAW)
    -- | (since Linux 2.6.28, macOS 10.12)
    --   Similar to 'Monotonic', but provides access to a
    --   raw hardware-based time that is not subject to NTP
    --   adjustments or the incremental adjustments performed by
    --   adjtime(3).
    --   @CLOCK_MONOTONIC_RAW@ (Windows - @QueryPerformanceCounter@, @QueryPerformanceFrequency@)
  | MonotonicRaw
#endif

#if defined (CLOCK_BOOTTIME)
    -- | (since Linux 2.6.39; Linux-specific)
    --   Identical to `Monotonic`, except it also includes
    --   any time that the system is suspended. This allows
    --   applications to get a suspend-aware monotonic clock
    --   without having to deal with the complications of 'Realtime',
    --   which may have discontinuities if the time is changed
    --   using settimeofday(2).
    --   (since Linux 4.17; identical to 'Monotonic')
    --   @CLOCK_BOOTTIME@
  | Boottime
#endif

#if defined (CLOCK_MONOTONIC_COARSE)
    -- | (since Linux 2.6.32; Linux-specific)
    --   A faster but less precise version of 'Monotonic'.
    --   Use when you need very fast, but not fine-grained timestamps.
    --   @CLOCK_MONOTONIC_COARSE@
  | MonotonicCoarse
#endif

#if defined (CLOCK_REALTIME_COARSE)
    -- | (since Linux 2.6.32; Linux-specific)
    --   A faster but less precise version of 'Realtime'.
    --   Use when you need very fast, but not fine-grained timestamps.
    --   @CLOCK_REALTIME_COARSE@
  | RealtimeCoarse
#endif

  deriving (Eq, Enum, Generic, Read, Show, Typeable)

#if defined(_WIN32)
foreign import ccall unsafe hs_clock_win32_gettime_monotonic :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_gettime_realtime :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_gettime_processtime :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_gettime_threadtime :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_getres_monotonic :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_getres_realtime :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_getres_processtime :: Ptr TimeSpec -> IO ()
foreign import ccall unsafe hs_clock_win32_getres_threadtime :: Ptr TimeSpec -> IO ()
#else
#if MIN_VERSION_base(4,10,0)
type ClockId = CClockId
#else
type ClockId = #{type clockid_t}
#endif

foreign import ccall unsafe clock_gettime :: ClockId -> Ptr TimeSpec -> IO CInt
foreign import ccall unsafe clock_getres  :: ClockId -> Ptr TimeSpec -> IO CInt

foreign import capi unsafe "time.h value CLOCK_MONOTONIC" clock_MONOTONIC :: ClockId
foreign import capi unsafe "time.h value CLOCK_REALTIME" clock_REALTIME :: ClockId
#if defined (CLOCK_PROCESS_CPUTIME_ID)
foreign import capi unsafe "time.h value CLOCK_PROCESS_CPUTIME_ID" clock_PROCESS_CPUTIME_ID :: ClockId
#endif
#if defined (CLOCK_THREAD_CPUTIME_ID)
foreign import capi unsafe "time.h value CLOCK_THREAD_CPUTIME_ID" clock_THREAD_CPUTIME_ID :: ClockId
#endif
#if defined (CLOCK_MONOTONIC_RAW)
foreign import capi unsafe "time.h value CLOCK_MONOTONIC_RAW" clock_MONOTONIC_RAW :: ClockId
#endif
#if defined (CLOCK_BOOTTIME)
foreign import capi unsafe "time.h value CLOCK_BOOTTIME" clock_BOOTTIME :: ClockId
#endif
#if defined (CLOCK_MONOTONIC_COARSE)
foreign import capi unsafe "time.h value CLOCK_MONOTONIC_COARSE" clock_MONOTONIC_COARSE :: ClockId
#endif
#if defined (CLOCK_REALTIME_COARSE)
foreign import capi unsafe "time.h value CLOCK_REALTIME_COARSE" clock_REALTIME_COARSE :: ClockId
#endif
#endif

#if !defined(_WIN32)
clockToConst :: Clock -> ClockId
clockToConst Monotonic = clock_MONOTONIC
clockToConst  Realtime = clock_REALTIME
#if defined (CLOCK_PROCESS_CPUTIME_ID)
clockToConst ProcessCPUTime = clock_PROCESS_CPUTIME_ID
#endif
#if defined (CLOCK_THREAD_CPUTIME_ID)
clockToConst  ThreadCPUTime = clock_THREAD_CPUTIME_ID
#endif
#if defined (CLOCK_MONOTONIC_RAW)
clockToConst    MonotonicRaw = clock_MONOTONIC_RAW
#endif
#if defined (CLOCK_BOOTTIME)
clockToConst        Boottime = clock_BOOTTIME
#endif
#if defined (CLOCK_MONOTONIC_COARSE)
clockToConst MonotonicCoarse = clock_MONOTONIC_COARSE
#endif
#if defined (CLOCK_REALTIME_COARSE)
clockToConst  RealtimeCoarse = clock_REALTIME_COARSE
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
#else
getTime clk = allocaAndPeek $! throwErrnoIfMinus1_ "clock_gettime" . clock_gettime (clockToConst clk)
#endif

#if defined(_WIN32)
getRes Monotonic = allocaAndPeek hs_clock_win32_getres_monotonic
getRes Realtime = allocaAndPeek hs_clock_win32_getres_realtime
getRes ProcessCPUTime = allocaAndPeek hs_clock_win32_getres_processtime
getRes ThreadCPUTime = allocaAndPeek hs_clock_win32_getres_threadtime
#else
getRes clk = allocaAndPeek $! throwErrnoIfMinus1_ "clock_getres" . clock_getres (clockToConst clk)
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
  (normalize -> TimeSpec xs xn) * (normalize -> TimeSpec ys yn) = normalize $! TimeSpec (s2ns*xs*ys+xs*yn+xn*ys) (xn*yn)
  negate (TimeSpec xs xn) = normalize $! TimeSpec (negate xs) (negate xn)
  abs    (normalize -> TimeSpec xs xn) | xs == 0   = normalize $! TimeSpec 0 xn
                                       | otherwise = normalize $! TimeSpec (abs xs) (signum xs * xn)
  signum (normalize -> TimeSpec xs xn) | xs == 0   = TimeSpec 0 (signum xn)
                                       | otherwise = TimeSpec 0 (signum xs)
  fromInteger x = TimeSpec (fromInteger q) (fromInteger r) where (q, r) = x `divMod` s2ns

instance Enum TimeSpec where
  succ x = x + 1
  pred x = x - 1
  toEnum x = normalize $ TimeSpec 0 (fromIntegral x)
  fromEnum = fromEnum . toInteger

instance Real TimeSpec where
  toRational x = toInteger x % 1

instance Integral TimeSpec where
  toInteger = toNanoSecs
  quot (toInteger-> t1) (toInteger-> t2) = fromInteger $! quot t1 t2
  rem (toInteger-> t1) (toInteger-> t2) = fromInteger $! rem t1 t2
  div (toInteger-> t1) (toInteger-> t2) = fromInteger $! div t1 t2
  mod (toInteger-> t1) (toInteger-> t2) = fromInteger $! mod t1 t2
  divMod (toInteger-> t1) (toInteger-> t2) =
    let (q,r)=divMod t1 t2 in (fromInteger $! q, fromInteger $! r)
  quotRem (toInteger-> t1) (toInteger-> t2) =
    let (q,r)=quotRem t1 t2 in (fromInteger $! q, fromInteger $! r)

instance Eq TimeSpec where
  (normalize -> TimeSpec xs xn) == (normalize -> TimeSpec ys yn) | True == es = xn == yn
                                                                 | otherwise  = es
                                                                   where   es = xs == ys

instance Ord TimeSpec where
  compare (normalize -> TimeSpec xs xn) (normalize -> TimeSpec ys yn) | EQ ==  os = compare xn yn
                                                                      | otherwise = os
                                                                        where  os = compare xs ys

instance Bounded TimeSpec where
  minBound = TimeSpec minBound 0
  maxBound = TimeSpec maxBound (s2ns-1)

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
