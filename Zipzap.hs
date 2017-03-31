{-# LANGUAGE TupleSections #-}
module Zipzap where
import Data.Tuple
import Data.Functor
import Control.Monad
import Prelude(uncurry, id, ($))

v = [0,1,2]
u = [0,3,6]


unzip   :: (Functor f) => f(a,b) -> (f a, f b)
unzip   = zip (fmap fst, fmap snd)

-- unzip . zip = id = zip . unzip
class Zippable f where zip     :: (f a, f b) -> f (a,b)

instance Zippable ((->) a) where
    zip (g, h) x = (g x, h x)


(.*) :: (Functor m, Monad m) => m a -> m b -> m (a,b)
ma .* mb = join $ fmap (\a -> fmap (a,) mb) ma

(*.) :: (Functor m, Monad m) => m a -> m b -> m (a,b)
ma *. mb = swap <$> mb .* ma

(<.*>) :: (Functor m, Monad m) => m (a->b) -> m a -> m b
mab <.*> ma = fmap (uncurry id) (mab .* ma)

(<*.>) :: (Functor m, Monad m) => m (a->b) -> m a -> m b
mab <*.> ma = fmap (uncurry id) (mab *. ma)