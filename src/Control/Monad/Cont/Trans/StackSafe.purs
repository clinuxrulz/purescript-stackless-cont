module Control.Monad.Cont.Trans.StackSafe
  ( ContT()
  , runContT
  ) where

import Prelude
import Control.Monad.Suspender.Trans
import Control.Monad.Cont.Class
import Control.Monad.Rec.Class
import Control.Monad.Trans
import Data.Either

newtype ContT r m a = ContT ((a -> SuspenderT m r) -> SuspenderT m r)

runContT :: forall r m a. (MonadRec m) => ContT r m a -> (a -> m r) -> m r
runContT (ContT ca) k = runSuspenderT $ ca (\a -> lift $ k a)

instance monadContContT :: (Monad m) => MonadCont (ContT r m) where
  callCC f = ContT (\k -> suspend (\_ -> case f (\a -> ContT (\_ -> suspend (\_ -> k a))) of ContT k2 -> k2 k))

instance functorContT :: (Monad m) => Functor (ContT r m) where
  map f (ContT ca) = ContT (\k -> suspend (\_ -> ca (\a -> suspend (\_ -> k $ f a))))

instance applyContT :: (Monad m) => Apply (ContT r m) where
  apply (ContT cf) (ContT ca) = ContT (\k -> suspend (\_ -> cf (\f -> suspend (\_ -> ca (\a -> suspend (\_ -> k $ f a))))))

instance applicativeContT :: (Monad m) => Applicative (ContT r m) where
  pure a = ContT (\k -> suspend (\_ -> k a))

instance bindContT :: (Monad m) => Bind (ContT r m) where
  bind (ContT ca) f = ContT (\k -> suspend (\_ -> ca (\a -> suspend (\_ -> case f a of ContT k2 -> k2 k))))

instance monadContT :: (Monad m) => Monad (ContT r m)

instance monadTransContT :: MonadTrans (ContT r) where
  lift m = ContT ((lift m) >>=)

instance monadRecContT :: (Monad m) => MonadRec (ContT r m) where
  tailRecM f a =
    f a >>= (\x -> ContT (\k ->
      suspend (\_ ->
        either
          (\y -> case tailRecM f y of ContT k2 -> k2 k)
          (\y -> k y)
          x
      )
    ))
