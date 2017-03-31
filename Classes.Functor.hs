{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Classes.Functor where
import qualified Prelude as P
import Imports
import Data.List(intersect)
import Control.Monad.Trans.State
import Zipzap (Zippable(..))
import Data.Functor ((<$))


(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = map

first = listToMaybe


class Lookup f p | f -> p where
    search      :: f a -> p -> Maybe a
    paths       :: (Eq a) => f a -> a -> [p]
    reachable   :: (Eq a) => f a -> a -> Bool

    reachable fa = not . P.null . paths fa

-- instance Lookup Tree [Int] where

class Functor f where
    map     :: (a -> b) -> f a -> f b
    shape   :: f a -> f ()

    shape f = map (P.const ()) f

class (Functor f) => CFunctor f c | f -> c where
    context :: f a -> f c
    cmap    :: (c -> a -> b) -> f a -> f b

    context  = cmap $ curry fst

class (Functor f) => Pointed f where
    unit    :: f ()
    pure    :: a -> f a

    pure x  = map (P.const x) unit

class (Pointed f) => Applicative f where
    (**)    :: f a -> f b -> f (a,b)
    ap      :: (a -> b) -> f a -> f b

    ap f fa = map (uncurry id) (pure f ** fa)

class (Applicative f) => Monad f where
    join    :: f (f a) -> f a
    bind    :: (a -> f b) -> f a -> f b
    (>>=)   :: f a -> (a -> f b) -> f b

    bind f  = join . map f
    (>>=)   = flip bind










instance Functor (State s)      where
    map f st  = state $ (\(a,s) -> (f a, s)) . runState st

instance CFunctor (State s) s   where
    cmap f st = state $ (\(a,s) -> (f s a, s)) . runState st

instance Pointed (State s)      where
    unit = state $ \s -> ((), s)

instance Applicative (State s)  where
    st0 ** st1 = state $ \s ->
        let (v1, s1) = runState st0 s
            (v2, s2) = runState st1 s1
        in  ((v1,v2), s2)

instance Monad (State s)        where
    join st = state $ \s0 -> uncurry runState (runState st s0)
