-- | High-resolution, realtime clock and timer functions for Posix
--   systems. This module is being developed according to IEEE Std
--   1003.1-2008: <http://www.opengroup.org/onlinepubs/9699919799/>,
--   <http://www.opengroup.org/onlinepubs/9699919799/functions/clock_getres.html#>

module System.Clock (

  Clock    (Monotonic, Realtime, ProcessCPUTime, ThreadCPUTime),
  TimeSpec (TimeSpec),

  getTime,
  getRes,

  sec,
  nsec

) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Applicative

-- | Clock types.
--   A clock may be system-wide (that is, visible to all processes)
--   or per-process (measuring time that is meaningful only within
--   a process). All implementations shall support CLOCK_REALTIME.
data Clock = Monotonic      -- ^ The identifier for the system-wide monotonic clock, which is defined as a clock measuring real time, whose value cannot be set via clock_settime and which cannot have negative clock jumps. The maximum possible clock jump shall be implementation-defined. For this clock, the value returned by 'getTime' represents the amount of time (in seconds and nanoseconds) since an unspecified point in the past (for example, system start-up time, or the Epoch). This point does not change after system start-up time. Note that the absolute value of the monotonic clock is meaningless (because its origin is arbitrary), and thus there is no need to set it. Furthermore, realtime applications can rely on the fact that the value of this clock is never set.
           | Realtime       -- ^ The identifier of the system-wide clock measuring real time. For this clock, the value returned by getTime represents the amount of time (in seconds and nanoseconds) since the Epoch.
           | ProcessCPUTime -- ^ The identifier of the CPU-time clock associated with the calling process. For this clock, the value returned by getTime represents the amount of execution time of the current process.
           | ThreadCPUTime  -- ^ The identifier of the CPU-time clock associated with the calling OS thread. For this clock, the value returned by getTime represents the amount of execution time of the current OS thread.

-- | TimeSpec structure
data TimeSpec = TimeSpec { sec  :: {-# UNPACK #-} !Int, -- ^ seconds
                           nsec :: {-# UNPACK #-} !Int  -- ^ nanoseconds
                         } deriving (Show, Read, Eq)

instance Storable TimeSpec where
  sizeOf    _ = sizeOf (0 :: Int) * 2
  alignment _ = 1
  poke t v    = do
    let i :: Ptr Int = castPtr t
    i |^ 0 $! sec  v
    i |^ 1 $! nsec v
  peek t      = do
    let i :: Ptr Int = castPtr t
    TimeSpec <$> i |. 0 <*> i |. 1

instance Ord TimeSpec where
  compare (TimeSpec xs xn) (TimeSpec ys yn) = 
    if      xs > ys then GT
    else if xs < ys then LT
    else if xn > yn then GT
    else if xn < yn then LT
    else EQ

-- | The 'getTime' function shall return the current value for the
--   specified clock.
getTime :: Clock -> IO TimeSpec
getTime = call . time

-- | The 'getRes' function shall return the resolution of any clock.
--   Clock resolutions are implementation-defined and cannot be set
--   by a process.
getRes :: Clock -> IO TimeSpec
getRes = call . res

---------------------------------------------

-- Reader function
type ReaderFunc = Ptr TimeSpec -> IO ()

-- Readers
foreign import ccall clock_readtime_monotonic   :: ReaderFunc
foreign import ccall clock_readtime_realtime    :: ReaderFunc
foreign import ccall clock_readtime_processtime :: ReaderFunc
foreign import ccall clock_readtime_threadtime  :: ReaderFunc

foreign import ccall clock_readres_monotonic    :: ReaderFunc
foreign import ccall clock_readres_realtime     :: ReaderFunc
foreign import ccall clock_readres_processtime  :: ReaderFunc
foreign import ccall clock_readres_threadtime   :: ReaderFunc

-- Clock-to-time reading
time :: Clock -> ReaderFunc
time Monotonic      = clock_readtime_monotonic
time Realtime       = clock_readtime_realtime
time ProcessCPUTime = clock_readtime_processtime
time ThreadCPUTime  = clock_readtime_threadtime

-- Clock-to-res reading
res :: Clock -> ReaderFunc
res Monotonic      = clock_readres_monotonic
res Realtime       = clock_readres_realtime
res ProcessCPUTime = clock_readres_processtime
res ThreadCPUTime  = clock_readres_threadtime

-- Marshalling
call :: ReaderFunc -> IO TimeSpec
call read_ = do
  x <- malloc
  read_     x
  t <- peek x
  free      x
  return    t

-- Allocation and pointer operations
{-# INLINE (|.) #-}; (|.)::Storable a=>Ptr a -> Int -> IO a         ; (|.) a i   = peekElemOff a i
{-# INLINE (|^) #-}; (|^)::Storable a=>Ptr a -> Int ->    a -> IO (); (|^) a i v = pokeElemOff a i v