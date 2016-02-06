module Control.Monad.Suspender.Class where

import Prelude

class (Monad m) <= MonadSuspender m where
  suspend :: forall a. (Unit -> m a) -> m a
