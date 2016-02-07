module Main where

import Prelude
import Control.Monad.Cont.Trans.StackSafe
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Rec.Class
import Data.Identity
import Data.Either
import Data.Tuple

main :: Eff (console :: CONSOLE) Unit
main = log $ show $ runIdentity $ runContT (tailRecM (\(Tuple a n) -> if n <= 10000 then pure $ Left $ Tuple (a+n) (n+1) else pure $ Right a) (Tuple 0 0)) pure
