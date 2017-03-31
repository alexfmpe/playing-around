{-# LANGUAGE TupleSections #-}
module Main where
import Control.Applicative
import Data.List
import qualified Data.Map as Map
import Data.Tree
import System.Random
import Deep
import Embed (innerHJoin, outterHJoin)
import Classes.Functor (cmap, context)
import Classes.Merge
import Form
import Learns
import Zipzap
import Zzz (cfilter)

main :: IO ()
main = do
    print $ innerHJoin ll
    print $ outterHJoin ll
    print $ innerHJoin lm
    print $ outterHJoin ml
    print $ map utopian [0,1,2,3,4,5,6]
    print $ map rtopian [0,1,2,3,4,5,6]
    print $ Merge.merge (pure . fst) pure pure (Just 7, Just 6)
    print $ Merge.merge pure (pure Nothing) (pure Nothing) (Just 8, Just 9)
    print $ Merge.merge (pure Nothing) pure (pure Nothing) ([0,1,2,3], [0,0])
--    print $ Merge.zip (Just 8, Just 9)
--    print $ Just 4  <|| Just 6
--    print $ [1,2,3] <|| [4,5,6,7]
    print $ showTree $ (,) <$> ta <*> pure 2
    print $ showTree $ (,) <$> ta <*> tb
    print $ [0,1,2] *. [0,3,6]
    print $ [0,1,2] .* [0,3,6]
    --print $ (*2) <<$>> ll

    print $ context [99, 99]
    print $ context $ Map.fromList [(1,2), (3,4)]
    print $ context (\x -> 4) 6
    print $ cmap replicate ['a', 'b', 'c']
    print $ cmap (const . length) ta
    print . showTree . context $ (,) <$> ta <*> tb

    print $ cfilter (\c a -> c > a) [4,3,2,1,0]
 {-
    d <- rollNDiceIO 4
    print d
    print . rollDice . mkStdGen $ 2
    --roflmaool
-}

showTree t = drawTree $ show <$> t

ta = Node 2  [Node 3  [], Node 5  [Node 7  []]]
tb = Node 11 [Node 13 [], Node 17 [Node 19 []]]

ll :: [[[Int]]]
ll = [[[1,2],[3,4]], [[10,20], [30,40]]]

lm :: [Maybe Int]
lm = [Just 0, Just 1, Nothing, Just 3]

ml :: Maybe [Int]
ml = Just [0,1,2]

utopian 0 = 1
utopian n = [(1+), (2*)] !! mod n 2 $ utopian (n-1)

rtopian n = reclist 1 [(2*), (1+)] !! n

--reclist base recs = go
--    where go 0 = base
--          go n = recs !! (mod n $ length recs) $ go (n-1)

reclist :: a -> [a->a] -> [a]
reclist base recs = base : (round ++ rest)
    where round = ($ base) . foldr (flip (.)) id <$> tail (inits recs)
          rest = tail $ reclist (last round) recs
