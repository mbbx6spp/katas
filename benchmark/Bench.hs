module Main (main) where

import           Criterion      (Benchmark, bench)
import           Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
  [ bgroup "base" benchmarks
  ]

benchmarks :: [Benchmark]
benchmarks = []
