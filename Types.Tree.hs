{-# LANGUAGE FlexibleInstances #-}
module Types.Tree where
import Imports
import Prelude()
import Classes.Functor
import Types.List
import Data.Tree

instance Functor Tree           where   map  = fmap
instance CFunctor Tree [Int]    where
    cmap f = go [] where
        go path (Node a ts) = Node (f path a) $ cmap (\i -> go $ path <> [i]) ts
        -- wtf infinite
        -- go path (Node a ts) = Node (f path a) $ cmap (go . (path <>) . pure) ts
