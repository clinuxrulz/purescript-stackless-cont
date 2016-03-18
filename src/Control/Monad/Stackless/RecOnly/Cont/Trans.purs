module Control.Monad.Stackless.RecOnly.Cont.Trans
  ( ContT()
  , runContT
  , mapContT
  , withContT
  ) where

import Prelude (class Monad, class Bind, class Applicative, class Apply, class Functor, Unit, (>>=), ($), (<$>), unit, pure)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans (class MonadTrans, lift)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader.Class (class MonadReader, local, ask)
import Control.Monad.State.Class (class MonadState, state)
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

-- | The CPS monad transformer.
-- | This one is like the stackless version, but only swaps stack-space for heap-space in tailRecM.
-- |
-- | This monad transformer extends the base monad with the operation `callCC`.
newtype ContT r m a = ContT ((a -> Suspender m r) -> Suspender m r)

-- | Run a computation in the `ContT` monad, by providing a continuation.
runContT :: forall r m a. (MonadRec m) => ContT r m a -> (a -> m r) -> m r
runContT (ContT ca) k = runSuspender $ ca (\a -> liftSuspender $ k a)

-- | Modify the underlying action in a `ContT` monad action.
mapContT :: forall r m a. (MonadRec m) => (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f (ContT ca) = ContT (\k -> suspend (\_ -> ca (\a -> liftSuspender $ f (runSuspender $ k a))))

kFromSuspender :: forall m a b. (MonadRec m) => (a -> Suspender m b) -> (a -> m b)
kFromSuspender k = (\a -> runSuspender $ k a)

kToSuspender :: forall m a b. (Functor m) => (a -> m b) -> (a -> Suspender m b)
kToSuspender k = (\a -> liftSuspender $ k a)

-- | Modify the continuation in a `ContT` monad action.
withContT :: forall r m a b. (MonadRec m) => ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f (ContT ca) = ContT (\k ->
  let k' = kToSuspender $ f $ kFromSuspender k
  in
  suspend (\_ -> ca (\a -> k' a))
)

instance monadContContT :: (Monad m) => MonadCont (ContT r m) where
  callCC f = ContT (\k -> case f (\a -> ContT (\_ -> k a)) of ContT k2 -> k2 k)

instance functorContT :: (Monad m) => Functor (ContT r m) where
  map f (ContT ca) = ContT (\k -> ca (\a -> k $ f a))

instance applyContT :: (Monad m) => Apply (ContT r m) where
  apply (ContT cf) (ContT ca) = ContT (\k -> cf (\f -> ca (\a -> k $ f a)))

instance applicativeContT :: (Monad m) => Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance bindContT :: (Monad m) => Bind (ContT r m) where
  bind (ContT ca) f = ContT (\k -> ca (\a ->case f a of ContT k2 -> k2 k))

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

instance monadEffContT :: (MonadEff eff m) => MonadEff eff (ContT r m) where
  liftEff m = lift $ liftEff m

instance monadReaderContT :: (MonadReader r1 m) => MonadReader r1 (ContT r2 m) where
  local f (ContT m) = ContT (\k -> m (\a -> Suspender $ (local f) $ unSuspender $ k a))
  ask = lift ask

instance monadStateContT :: (MonadState s m) => MonadState s (ContT s m) where
  state f = lift $ state f
