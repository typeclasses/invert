import Criterion.Main
import Criterion.Types

import qualified Invert as I

sizes = [100, 500, 2_000, 5_000] :: [Integer]

invertGroup name strategy =
    bgroup name $
        fmap (invertBench strategy) sizes

invertBench strategy size =
    bench (show size) $
        let
            f = I.function strategy [1 .. size] (* (2 ^ 10))
            f' x = sum (foldMap (\e -> f (x ^ e)) [0 .. 10])
        in
            whnf f' size

groups =
    [ invertGroup "linearSearchLazy" I.linearSearchLazy
    , invertGroup "linearSearchStrict" I.linearSearchStrict
    , invertGroup "binarySearch" I.binarySearch
    , invertGroup "hashTable" I.hashTable
    ]

config =
    defaultConfig
        { reportFile = Just "bench.html"
        }

main = defaultMainWith config groups
