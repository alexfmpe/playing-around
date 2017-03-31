{-# LANGUAGE FlexibleInstances #-}
module Types.Map where
import Imports
import Prelude()
import Classes.Functor
import Data.Map

instance Functor (Map k)        where   map  = Data.Map.map
instance CFunctor (Map k) k     where   cmap = mapWithKey
