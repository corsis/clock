module Main (main) where

import Criterion.Main
import System.Clock

main :: IO ()
main = defaultMain [
    bgroup "getTime" [
        bench "Monotonic" $ whnfIO (getTime Monotonic)
      , bench "Realtime" $ whnfIO (getTime Realtime)
      , bench "ProcessCPUTime" $ whnfIO (getTime ProcessCPUTime)
      , bench "ThreadCPUTime" $ whnfIO (getTime ThreadCPUTime)
      , bench "MonotonicRaw" $ whnfIO (getTime MonotonicRaw)
      , bench "Boottime" $ whnfIO (getTime Boottime)
      , bench "MonotonicCoarse" $ whnfIO (getTime MonotonicCoarse)
      , bench "RealtimeCoarse" $ whnfIO (getTime RealtimeCoarse)
      ]
  ]
