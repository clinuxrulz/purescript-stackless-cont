module Control.Monad.Cont.StackSafe
  ( Cont()
  , runCont
  ) where

import Prelude
import Control.Monad.Cont.Trans.StackSafe
import Data.Identity

type Cont r a = ContT r Identity a

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont c k = runIdentity $ runContT c (\a -> pure $ k a)
