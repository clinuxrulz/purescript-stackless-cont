module Main where

import Prelude
import Control.Plus
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Cont.Trans.StackSafe
import Control.Monad.Cont.Class
import Control.Monad.Trans
import Control.MonadPlus
import Data.Maybe

newtype List a = List (forall r. ContT r Maybe a)

unList :: forall a. List a -> (forall r. ContT r Maybe a)
unList (List a) = a

instance functorList :: Functor List where
  map f (List ca) = List (f <$> ca)

instance applyList :: Apply List where
  apply (List cf) (List ca) = List (cf <*> ca)

instance applicativeList :: Applicative List where
  pure a = List (pure a)

instance bindList :: Bind List where
  bind (List ca) f = List (ca >>= (unList <<< f))

instance monadList :: Monad List

bail :: forall r a. ContT r Maybe a
bail = lift Nothing

-- list with elements 1 and 2.
test :: List Int
test =
  List (
    callCC
      (\k -> do
        k 1
        k 2
        bail
      )
  )

range :: Int -> Int -> List Int
range from to = List (
    callCC (\k ->
      let go =
            (\a -> do
              k a
              case compare a to of
                LT -> go (a+1)
                EQ -> bail
                GT -> go (a-1)
            )
      in
      go from
    )
  )

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Hello sailor!"
