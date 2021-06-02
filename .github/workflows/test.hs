import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.6.5"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.6.0.1"
                  , "--constraint=criterion == 1.5.0.0"
                  , "--constraint=hashable == 1.2.7.0"
                  , "--constraint=generic-deriving == 1.14"
                  , "--constraint=unordered-containers == 0.2.9.0"
                  , "--constraint=vector == 0.12.0.1"
                  ]
      "8.8.4"  -> callProcess "cabal" ["test", "all"]
      "8.10.3" -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.6.4.1"
                  , "--constraint=criterion == 1.5.9.0"
                  , "--constraint=hashable == 1.3.2.0"
                  , "--constraint=generic-deriving == 1.14"
                  , "--constraint=unordered-containers == 0.2.14.0"
                  , "--constraint=vector == 0.12.3.0"
                  ]
