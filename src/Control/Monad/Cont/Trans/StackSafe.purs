module Control.Monad.Cont.Trans.StackSafe
  ( ContT()
  , runContT
  ) where

import Prelude
import Control.Monad.Suspender.Class
import Control.Monad.Cont.Class
import Control.Monad.Rec.Class
import Data.Either

newtype ContT r m a = ContT ((a -> m r) -> m r)

runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT ca) k = ca k

instance monadContContT :: (MonadSuspender m) => MonadCont (ContT r m) where
  callCC f = ContT (\k -> suspend (\_ -> case f (\a -> ContT (\_ -> suspend (\_ -> k a))) of ContT k2 -> k2 k))

instance functorContT :: (MonadSuspender m) => Functor (ContT r m) where
  map f (ContT ca) = ContT (\k -> suspend (\_ -> ca (\a -> suspend (\_ -> k $ f a))))

instance applyContT :: (MonadSuspender m) => Apply (ContT r m) where
  apply (ContT cf) (ContT ca) = ContT (\k -> suspend (\_ -> cf (\f -> suspend (\_ -> ca (\a -> suspend (\_ -> k $ f a))))))

instance applicativeContT :: (MonadSuspender m) => Applicative (ContT r m) where
  pure a = ContT (\k -> suspend (\_ -> k a))

instance bindContT :: (MonadSuspender m) => Bind (ContT r m) where
  bind (ContT ca) f = ContT (\k -> suspend (\_ -> ca (\a -> suspend (\_ -> case f a of ContT k2 -> k2 k))))

instance monadContT :: (MonadSuspender m) => Monad (ContT r m)

liftSuspender :: forall r m a. (MonadSuspender m) => m a -> ContT r m a
liftSuspender m = ContT (m >>=)

instance monadRecContT :: (MonadSuspender m) => MonadRec (ContT r m) where
  tailRecM f a =
    f a >>= (\x -> ContT (\k ->
      suspend (\_ ->
        either
          (\y -> case tailRecM f y of ContT k2 -> k2 k)
          (\y -> k y)
          x
      )
    ))
