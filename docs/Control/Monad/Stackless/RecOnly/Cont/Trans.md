## Module Control.Monad.Stackless.RecOnly.Cont.Trans

#### `ContT`

``` purescript
newtype ContT r m a
```

The CPS monad transformer.
This one is like the stackless version, but only swaps stack-space for heap-space in tailRecM.

This monad transformer extends the base monad with the operation `callCC`.

##### Instances
``` purescript
(Monad m) => MonadCont (ContT r m)
(Monad m) => Functor (ContT r m)
(Monad m) => Apply (ContT r m)
(Monad m) => Applicative (ContT r m)
(Monad m) => Bind (ContT r m)
(Monad m) => Monad (ContT r m)
MonadTrans (ContT r)
(Monad m) => MonadRec (ContT r m)
(MonadEff eff m) => MonadEff eff (ContT r m)
(MonadReader r1 m) => MonadReader r1 (ContT r2 m)
(MonadState s m) => MonadState s (ContT s m)
```

#### `runContT`

``` purescript
runContT :: forall r m a. (MonadRec m) => ContT r m a -> (a -> m r) -> m r
```

Run a computation in the `ContT` monad, by providing a continuation.

#### `mapContT`

``` purescript
mapContT :: forall r m a. (MonadRec m) => (m r -> m r) -> ContT r m a -> ContT r m a
```

Modify the underlying action in a `ContT` monad action.

#### `withContT`

``` purescript
withContT :: forall r m a b. (MonadRec m) => ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
```

Modify the continuation in a `ContT` monad action.


