module Control.Monad.Suspender.Class where

import Prelude
import Data.Identity

class (Monad m) <= MonadSuspender m where
  suspend :: forall a. (Unit -> m a) -> m a

instance monadSuspenderIdentity :: MonadSuspender Identity where
  suspend f = f unit
