import Criterion.Main (Benchmark, bench, bgroup, defaultConfig, defaultMainWith, whnf)
import Criterion.Types (Config (reportFile))
import Invert qualified as I
import Prelude

sizes :: [Integer]
sizes = [100, 500, 2_000, 5_000] :: [Integer]

invertGroup :: String -> I.Strategy Integer Integer -> Benchmark
invertGroup name strategy =
  bgroup name $
    fmap (invertBench strategy) sizes

invertBench :: (Show b, Enum b, Num b) => I.Strategy b b -> b -> Benchmark
invertBench strategy size =
  bench (show size) $
    let f = I.function strategy [1 .. size] (* (2 ^ (10 :: Integer)))
        f' x = sum (foldMap (\e -> f (x ^ (e :: Integer))) [0 .. 10])
     in whnf f' size

groups :: [Benchmark]
groups =
  [ invertGroup "linearSearchLazy" I.linearSearchLazy,
    invertGroup "linearSearchStrict" I.linearSearchStrict,
    invertGroup "binarySearch" I.binarySearch,
    invertGroup "hashTable" I.hashTable
  ]

config :: Config
config =
  defaultConfig
    { reportFile = Just "bench.html"
    }

main :: IO ()
main = defaultMainWith config groups
