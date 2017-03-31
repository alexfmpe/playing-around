{-# LANGUAGE FlexibleInstances #-}
module Types.Function where
import Imports
import Prelude()
import Classes.Functor

instance Functor     ((->) x)   where map f g = f . g
instance Pointed     ((->) x)   where unit x = ()
instance Applicative ((->) x)   where (**) f g x = (f x, g x)
instance Monad       ((->) x)   where join f x = f x x
instance CFunctor    ((->) x) x where cmap f g x = f x (g x)
