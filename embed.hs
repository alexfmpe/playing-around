{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Embed(
    embed,
    innerHJoin,
    outterHJoin,
) where

import Data.Map
import Control.Monad


class Embed f g where
    embed :: f -> g

innerHJoin  :: (Embed (f a) (g a),
                Monad g, Functor g) => g (f a) -> g a
innerHJoin  = join . fmap embed

outterHJoin :: (Embed (f (g a)) (g (g a)),
                Monad g, Functor g) => f (g a) -> g a
outterHJoin = join . embed

instance Embed a a where
    embed = id

--instance (Embed a b, Embed b c) => Embed a c where
--    embed = embed . embed

instance Embed (Maybe a) (Either () a) where
    embed (Just x) = Right x
    embed Nothing  = Left ()

instance Embed (Maybe a) [a] where
    embed (Just x) = [x]
    embed Nothing  = []

instance Embed [a] (Map Int a) where
    embed = Data.Map.fromList . zip [0..]

instance Embed (Map Int a) ((->) Int (Maybe a)) where
    embed = flip Data.Map.lookup
