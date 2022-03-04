module Test.Main where

import Prelude
import Control.Monad.Stackless.Cont.Trans
import Control.Monad.Rec.Class
import Effect
import Effect.Console
import Control.Monad.Trans.Class
import Data.Tuple

main :: Effect Unit
main = do
  _ <- runContT
    (tailRecM
      (\(Tuple a n) -> do
        if n `mod` 1000 == 0
          then do
            lift $ log $ "upto: " <> (show n)
            pure unit
          else pure unit
        if n <= 1000000
          then pure $ Loop $ Tuple (a+n) (n+1)
          else do
            lift $ log "done."
            pure $ Done $ a
      )
      (Tuple 0 0)
    )
    pure
  pure unit

