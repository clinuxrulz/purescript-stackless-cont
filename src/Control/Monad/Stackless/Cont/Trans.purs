module Control.Monad.Stackless.Cont.Trans
  ( ContT()
  , runContT
  ) where

import Prelude (class Monad, class Bind, class Applicative, class Apply, class Functor, Unit, (>>=), ($), (<$>), unit, pure)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (class MonadTrans)
import Data.Either (Either(Right, Left), either)

newtype Suspender m a = Suspender (m (Either (Unit -> Suspender m a) a))

unSuspender :: forall m a. Suspender m a -> m (Either (Unit -> Suspender m a) a)
unSuspender (Suspender a) = a

suspend :: forall m a. (Applicative m) => (Unit -> Suspender m a) -> Suspender m a
suspend f = Suspender $ pure $ Left f

done :: forall m a. (Applicative m) => a -> Suspender m a
done a = Suspender $ pure $ Right a

runSuspender :: forall m a. (MonadRec m) => Suspender m a -> m a
runSuspender s = tailRecM go s
  where
    go :: Suspender m a -> m (Either (Suspender m a) a)
    go (Suspender a) = (either (\x -> Left $ x unit) (\x -> Right $ x)) <$> a

liftSuspender :: forall m a. (Functor m) => m a -> Suspender m a
liftSuspender m = Suspender (Right <$> m)

newtype ContT r m a = ContT ((a -> Suspender m r) -> Suspender m r)

runContT :: forall r m a. (MonadRec m) => ContT r m a -> (a -> m r) -> m r
runContT (ContT ca) k = runSuspender $ ca (\a -> liftSuspender $ k a)

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
  lift m = ContT (\k -> Suspender $ m >>= (\a -> unSuspender $ k a))

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
