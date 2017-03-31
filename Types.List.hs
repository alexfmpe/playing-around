module Types.List where
import Imports
import Prelude()
import Classes.Functor
import Classes.Merge
import Filter

instance Lookup [] Int where
    search xs n = first $ drop n xs
    paths  xs y = map fst $ filter ((True ==) . snd) $ cmap (,) $ (y ==) <$> xs

instance Functor        []      where
    map f []     = []
    map f (x:xs) = f x : map f xs

instance CFunctor [] Int        where   cmap f = zipWith f [0..]
instance Pointed []             where   unit = [()]

instance Mergeable [] where
    pairs ([], [])  = []
    pairs (xs, ys)  = (listToMaybe xs, listToMaybe ys) : pairs (drop 1 xs, drop 1 ys)
