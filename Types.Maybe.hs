module Types.Maybe where
import Imports
import Prelude()
import Classes.Functor
import Classes.Merge

instance Lookup Maybe Bool where
    search mx True  = mx
    search _  False = Nothing

    paths (Just x) y = if x == y then [True] else []
    paths _        _ = []

instance Pointed        Maybe   where unit = Just ()

instance Functor        Maybe   where
    map f (Just x)  = Just (f x)
    map _ _         = Nothing

instance CFunctor Maybe Bool    where
    cmap f Nothing  = Nothing
    cmap f x = f True <$> x

instance Applicative    Maybe   where
    (Just x) ** (Just y) = Just (x, y)
    _ ** _               = Nothing

instance Monad          Maybe   where
    join (Just (Just x)) = Just x
    join _               = Nothing

