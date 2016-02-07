module Test.Main where

import Prelude
import Control.Monad.Cont.Trans.StackSafe
import Control.Monad.Rec.Class
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Trans
import Data.Tuple
import Data.Either

main :: Eff (console :: CONSOLE) Unit
main = do
  runContT
    (tailRecM
      (\(Tuple a n) -> do
        if n `mod` 1000 == 0
          then do
            lift $ log $ "upto: " ++ (show n)
            return unit
          else return unit
        if n <= 1000000
          then return $ Left $ Tuple (a+n) (n+1)
          else do
            lift $ log "done."
            return $ Right $ a
      )
      (Tuple 0 0)
    )
    pure
  return unit

