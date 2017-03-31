{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Classes.Merge (
    Mergeable(..),
)
where
import Imports
import Prelude()
import Filter
import Classes.Functor
import Zipzap
import Types.Function

dup :: a -> (a,a)
dup x = (x,x)

{-
LAWS
(fmap prune *** fmap prune) . unzip . pairs = id
empty = prune (Nothing <$ ma)
-}

instance Mergeable Maybe where
    pairs = Just
    merge gxy gx gy = go where
        go (Just x, Just y) = gxy (x, y)
        go (Just x, _     ) = gx x
        go (_     , Just y) = gy y
        go _                = Nothing

class (Functor f, Filter f) => Mergeable f where
    pairs   :: (f a, f b) -> f (Maybe a, Maybe b)
    merge   :: ((a, b) -> Maybe c) -> (a -> Maybe c) -> (b -> Maybe c) -> (f a, f b) -> f c

    zup     :: (f a, f a) -> f(a,a)
    (∆)     :: f a -> f a -> f a
    (\\)    :: f a -> f a -> f a
    (|\)    :: f a -> f a -> f a
    (|/)    :: f a -> f a -> f a
    (/|)    :: f a -> f a -> f a
    (\|)    :: f a -> f a -> f a

    merge gxy gx gy fxfy = prune $ map (merge gxy gx gy) (pairs fxfy)
    zup     = merge Just (Just . dup) (Just . dup)
    (∆)     = curry $ merge (pure Nothing) Just Just
    (\\)    = curry $ merge (pure Nothing) Just (pure Nothing)
    (|\)    = curry $ merge (Just . fst) Just Just
    (|/)    = curry $ merge (Just . fst) (pure Nothing) (pure Nothing)
    a /| b  = b |\ a
    a \| b  = b |/ a

instance (Mergeable f) => Zippable f where
    zip     = merge Just (pure Nothing) (pure Nothing)


-- instance (Ord k) => Mergeable (Map k) where
--    fzip = uncurry $ intersectionWith (,)
