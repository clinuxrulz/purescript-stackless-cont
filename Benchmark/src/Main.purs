module Main where

import Prelude

import Control.Monad.Cont.Trans.StackSafe
import Control.Monad.Eff
import Control.Monad.Free
import Control.Monad.Trampoline

import Data.Array
import Data.Foldable
import Data.Identity

import Benchotron.Core
import Benchotron.UI.Console

import Test.QuickCheck.Gen (vectorOf)

leftBindSmallBenchmark :: Benchmark
leftBindSmallBenchmark = mkBenchmark
  { slug: "left-bind-small"
  , title: "Left associated binds (small - " ++ show inputsPerSize ++ " inputs per size)"
  , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
  , sizeInterpretation: "Number of binds"
  , inputsPerSize: inputsPerSize
  , gen: \n -> vectorOf n (pure 0.0)
  , functions: [ benchFn "Free" (runTrampoline <<< binds)
               , benchFn "ContT" (\as -> runContT (contBinds as) pure)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampoline a
  gen = suspend <<< done

  contBinds :: Array Number -> ContT Number Identity Number
  contBinds as = foldl (\b a -> b >>= const (contGen a)) (contGen 0.0) as

  contGen :: forall a. a -> ContT Number Identity a
  contGen = pure

rightBindSmallBenchmark :: Benchmark
rightBindSmallBenchmark = mkBenchmark
  { slug: "right-bind-small"
  , title: "Right associated binds (small - " ++ show inputsPerSize ++ " inputs per size)"
  , sizes: [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 250, 500, 1000]
  , sizeInterpretation: "Number of binds"
  , inputsPerSize: inputsPerSize
  , gen: \n -> vectorOf n (pure 0.0)
  , functions: [ benchFn "Free" (runTrampoline <<< binds)
               , benchFn "ContT" (\as -> runContT (contBinds as) pure)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 100

  binds :: Array Number -> Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampoline a
  gen = suspend <<< done

  contBinds :: Array Number -> ContT Number Identity Number
  contBinds as = foldl (\b a -> contGen a >>= const b) (contGen 0.0) as

  contGen :: forall a. a -> ContT Number Identity a
  contGen = pure

leftBindLargeBenchmark :: Benchmark
leftBindLargeBenchmark = mkBenchmark
  { slug: "left-bind-large"
  , title: "Left associated binds (large - " ++ show inputsPerSize ++ " input per size)"
  , sizes: [1, 5, 10, 15, 20, 25, 30 ] <#> (* 100000)
  , sizeInterpretation: "Number of binds"
  , inputsPerSize: inputsPerSize
  , gen: \n -> vectorOf n (pure 0.0)
  , functions: [ benchFn "Free" (runTrampoline <<< binds)
               , benchFn "ContT" (\as -> runContT (contBinds as) pure)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampoline Number
  binds as = foldl (\b a -> b >>= const (gen a)) (gen 0.0) as

  gen :: forall a. a -> Trampoline a
  gen = suspend <<< done

  contBinds :: Array Number -> ContT Number Identity Number
  contBinds as = foldl (\b a -> b >>= const (contGen a)) (contGen 0.0) as

  contGen :: forall a. a -> ContT Number Identity a
  contGen = pure

rightBindLargeBenchmark :: Benchmark
rightBindLargeBenchmark = mkBenchmark
  { slug: "right-bind-large"
  , title: "Right associated binds (large - " ++ show inputsPerSize ++ " input per size)"
  , sizes: [1, 5, 10, 15, 20, 25, 30 ] <#> (* 100000)
  , sizeInterpretation: "Number of binds"
  , inputsPerSize: inputsPerSize
  , gen: \n -> vectorOf n (pure 0.0)
  , functions: [ benchFn "Free" (runTrampoline <<< binds)
               , benchFn "ContT" (\as -> runContT (contBinds as) pure)
               ]
  }
  where
  inputsPerSize :: Int
  inputsPerSize = 1

  binds :: Array Number -> Trampoline Number
  binds as = foldl (\b a -> gen a >>= const b) (gen 0.0) as

  gen :: forall a. a -> Trampoline a
  gen = suspend <<< done

  contBinds :: Array Number -> ContT Number Identity Number
  contBinds as = foldl (\b a -> contGen a >>= const b) (contGen 0.0) as

  contGen :: forall a. a -> ContT Number Identity a
  contGen = pure

main = runSuite [ leftBindSmallBenchmark
                , rightBindSmallBenchmark
                , leftBindLargeBenchmark
                , rightBindLargeBenchmark
                ]
