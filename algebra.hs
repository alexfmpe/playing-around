{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Algebra where

import qualified Prelude
import Prelude (Int, Bool, Rational, Integer, (.), ($),
                Eq, Ord, Read, Show, Num)
import Data.Map
import Data.Set
import Data.Foldable

data Order = LT | EQ | GT
data PreOrder = Maybe Order

-- class Poset
-- class Chain

class Wrapper w where
    wrap   :: a -> w a
    unwrap :: w a -> a
    fmap   :: (a -> b) -> w a -> w b
    fmap2  :: (a -> b -> c) -> w a -> w b -> w c

    fmap  f = wrap . f . unwrap
    fmap2 f x y = wrap $ f (unwrap x) (unwrap y)

newtype Add' a = Add' { getAdd :: a } deriving (Eq, Ord, Read, Show)
newtype Mul' a = Mul' { getMul :: a } deriving (Eq, Ord, Read, Show)
newtype Inf' a = Inf' { getInf :: a } deriving (Eq, Ord, Read, Show)
newtype Sup' a = Sup' { getSup :: a } deriving (Eq, Ord, Read, Show)

instance Wrapper Add' where wrap = Add' ; unwrap = getAdd
instance Wrapper Mul' where wrap = Mul' ; unwrap = getMul
instance Wrapper Inf' where wrap = Inf' ; unwrap = getInf
instance Wrapper Sup' where wrap = Sup' ; unwrap = getSup

instance (Add a) => Magma (Add' a) where (<>) = fmap2 (+)
instance (Mul a) => Magma (Mul' a) where (<>) = fmap2 (*)
instance (Inf a) => Magma (Inf' a) where (<>) = fmap2 (/\)
instance (Sup a) => Magma (Sup' a) where (<>) = fmap2 (\/)

instance MonoidAdd a => Monoid (Add' a) where id = Add' zero
instance MonoidMul a => Monoid (Mul' a) where id = Mul' one
instance MonoidInf a => Monoid (Inf' a) where id = Inf' max
instance MonoidSup a => Monoid (Sup' a) where id = Sup' min

instance GroupAdd  a => Group  (Add' a) where inverse = fmap negative
instance GroupMul  a => Group  (Mul' a) where inverse = fmap reciprocal

class Magma a where (<>) :: a -> a -> a
class Add   a where (+)  :: a -> a -> a
class Mul   a where (*)  :: a -> a -> a
class Sup   a where (\/) :: a -> a -> a
class Inf   a where (/\) :: a -> a -> a

class Magma a => Monoid    a where id   :: a
class Add   a => MonoidAdd a where zero :: a
class Mul   a => MonoidMul a where one  :: a
class Sup   a => MonoidSup a where min  :: a
class Inf   a => MonoidInf a where max  :: a

class Monoid    a => Group      a where inverse    :: a -> a
class MonoidAdd a => GroupAdd   a where negative   :: a -> a
class MonoidMul a => GroupMul   a where reciprocal :: a -> a

class (Sup a, Inf a, Metric a a) => Difference a where
    difference :: a -> a -> a
    difference a b = distance (a \/ b) (a /\ b)

class (GroupAdd    a, MonoidMul a) => RingArithmetic a
class (GroupAdd    a, GroupMul  a) => FieldArithmetic a
class (Difference  a, MonoidInf a) => RingBoolean a


class (Ord b) => Metric a b where distance :: a -> a -> b

instance Metric Bool Bool   where distance = (Prelude./=)
instance Metric Int  Int    where distance = Prelude.subtract
instance (Ord a, Ord k) => Metric (Map k a) (Map k a) where distance = Data.Map.difference
instance (Ord a)        => Metric (Set a)   (Set a)   where distance = Data.Set.difference

instance Magma      [a]         where (<>) = (Prelude.++)
instance Magma      (a->a)      where (<>) = (Prelude..)

instance Add Int        where (+)  = (Prelude.+)
instance Mul Int        where (*)  = (Prelude.*)
instance Sup Int        where (\/) = Prelude.max
instance Inf Int        where (/\) = Prelude.min

instance Add Rational   where (+)  = (Prelude.+)
instance Mul Rational   where (*)  = (Prelude.*)
instance Sup Rational   where (\/) = Prelude.max
instance Inf Rational   where (/\) = Prelude.min

instance Sup Bool       where (\/) = (Prelude.||)
instance Inf Bool       where (/\) = (Prelude.&&)

instance (Ord k) => Sup (Map k a)  where (\/) = Data.Map.union
instance (Ord k) => Inf (Map k a)  where (/\) = Data.Map.intersection

instance (Ord a) => Sup (Set a)    where (\/) = Data.Set.union
instance (Ord a) => Inf (Set a)    where (/\) = Data.Set.intersection

instance Monoid         [a]         where id    = []
instance Monoid         (a->a)      where id    = Prelude.id
instance MonoidAdd      Int         where zero  = 0
instance MonoidMul      Int         where one   = 1
instance MonoidAdd      Rational    where zero  = 0
instance MonoidMul      Rational    where one   = 1
instance MonoidSup      Bool        where min = Prelude.False
instance MonoidInf      Bool        where max  = Prelude.True
instance (Ord k) => MonoidSup (Map k a)   where min = Data.Map.empty
instance (Ord a) => MonoidSup (Set a)     where min = Data.Set.empty

instance Difference Bool
instance Difference Int
instance (Ord a) => Difference (Set a)
instance (Ord a, Ord k) => Difference (Map k a)

instance GroupAdd           Int         where negative = Prelude.negate
instance RingArithmetic     Int

instance GroupAdd           Rational    where negative   = Prelude.negate
instance GroupMul           Rational    where reciprocal = Prelude.recip
instance RingArithmetic     Rational
instance FieldArithmetic    Rational

--class (MonoidSup a, MonoidInf a, Eq a) => Lattice a
--instance (Lattice a) => Ord a where
--    (<=) =

--sums :: (MonoidAdd a, Foldable f) => f a -> a
--sums as = Data.Foldable.foldr (+) as

data (Ord a) => Interval a = Interval a a Bool Bool

joinIntervals :: (Ord a) => (a -> a -> a) -> (a -> a -> a) -> Interval a -> Interval a -> Interval a
joinIntervals f g (Interval minA maxA minOpenA maxOpenA) (Interval minB maxB minOpenB maxOpenB) =
        Interval (f minA minB) (g maxA maxB) (minOpenA \/ minOpenB) (maxOpenA \/ maxOpenB)

instance (Ord a, Add a)         => Add (Interval a) where (+)  = joinIntervals (+)   (+)
instance (Ord a, Mul a)         => Mul (Interval a) where (*)  = joinIntervals (*)   (*)
instance (Ord a, Inf a, Sup a)  => Inf (Interval a) where (/\) = joinIntervals (\/) (/\)
instance (Ord a, Inf a, Sup a)  => Sup (Interval a) where (\/) = joinIntervals (/\) (\/)
