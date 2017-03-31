module Filter where

import Imports
import qualified Prelude as P
import Classes.Functor

class Filter f where
    empty   :: f a
  --full?   :: f a
    prune   :: f (Maybe a) -> f a
    filter  :: (a -> Bool) -> f a -> f a


--  filter p fa = prune $ map (\a -> if p a then Just a else Nothing) fa
--  empty  :: f a
--  empty = filter (pure False) unit

instance Filter Maybe           where
    empty = Nothing

    prune (Just j) = j
    prune Nothing  = Nothing

    filter p fa = prune $ P.fmap (\a -> if p a then Just a else Nothing) fa

instance Filter []              where
    empty = []

    prune []            = []
    prune (Nothing:xs)  = prune xs
    prune (Just x : xs) = x : prune xs

    filter p fa = prune $ P.fmap (\a -> if p a then Just a else Nothing) fa
