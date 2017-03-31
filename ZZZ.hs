module Zzz where
import Prelude()
import Imports
import Classes.Functor
import Filter
import Types.List

cfilter :: (Filter f, CFunctor f c) => (c -> a -> Bool) -> f a -> f a
cfilter p = map fst . filter snd . cmap (\c a -> (a, p c a))

list :: (Functor f, Lookup f p) => f a -> [a]
list fa = prune $ map (search fa) $ paths (shape fa) ()

substructure :: (Filter f, Lookup f p, Eq a) => f a -> [p] -> f a
substructure fa ps = filter (`elem` reach) fa
    where reach = catMaybes $ search fa <$> ps
