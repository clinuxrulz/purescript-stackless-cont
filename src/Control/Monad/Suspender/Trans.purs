module Control.Monad.Suspender.Trans
  ( SuspenderF(..)
  , SuspenderT()
  , unSuspenderT
  , suspend
  , runSuspenderT
  ) where

import Prelude
import Control.Bind
import Control.Monad.Free.Trans
import Control.Monad.Suspender.Class (MonadSuspender)
import Control.Monad.Rec.Class
import Control.Monad.Trans

data SuspenderF (m :: * -> *) a
  = Suspend (Unit -> SuspenderT m a)

instance functorSuspenderF :: (Monad m) => Functor (SuspenderF m) where
  map f (Suspend k) = Suspend ((map f) <<< k)

newtype SuspenderT m a = SuspenderT (FreeT (SuspenderF m) m a)

unSuspenderT :: forall m a. SuspenderT m a -> FreeT (SuspenderF m) m a
unSuspenderT (SuspenderT a) = a

suspend :: forall m a. (Monad m) => (Unit -> SuspenderT m a) -> SuspenderT m a
suspend f = SuspenderT $ liftFreeT (Suspend f)

runSuspenderT :: forall m a. (MonadRec m) => SuspenderT m a -> m a
runSuspenderT (SuspenderT a) = runFreeT go a
  where
    go :: (SuspenderF m) (FreeT (SuspenderF m) m a) -> m (FreeT (SuspenderF m) m a)
    go (Suspend f) = pure $ join $ unSuspenderT $ f unit

instance functorSuspenderT :: (Monad m) => Functor (SuspenderT m) where
  map f = SuspenderT <<< (map f) <<< unSuspenderT

instance applySuspenderT :: (Monad m) => Apply (SuspenderT m) where
  apply (SuspenderT sf) (SuspenderT sa) = SuspenderT $ sf <*> sa

instance applicativeSuspenderT :: (Monad m) => Applicative (SuspenderT m) where
  pure a = SuspenderT $ pure a

instance bindSuspenderT :: (Monad m) => Bind (SuspenderT m) where
  bind (SuspenderT m) f = SuspenderT $ m >>= (unSuspenderT <<< f)

instance monadSuspenderT :: (Monad m) => Monad (SuspenderT m)

instance monadTransSuspenderT :: MonadTrans SuspenderT where
  lift m = SuspenderT $ lift m

instance monadSuspenderSuspenderT :: (Monad m) => MonadSuspender (SuspenderT m) where
  suspend = suspend
