{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)
import Control.Applicative (liftA2)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (x,y) <= Pointwise (x',y') = x <= x' && y <= y' 
    

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  Lexicographic (x,y) <= Lexicographic (x',y') =
    x < x' || x == x' && y <= y' 

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  Fun f <> Fun g = Fun $ liftA2 (<>) f g

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun $ const mempty 

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  First (Just x) <> _ = First $ Just x
  _ <> x = x

instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing 

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  _ <> Last (Just x) = Last (Just x)
  x <> _ = x

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing 

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (x,y) <> Pair (x',y') = Pair (x <> x',y <> y')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty,mempty) 

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  Dual x <> Dual x' = Dual (x' <> x) 

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty 

reverse :: [a] -> [a]
-- reverse = getDual . foldMap (Dual . (:[]))
-- this shoud be faster (foldMap uses foldr for []):
reverse = getDual . foldl (\acc x -> acc <> Dual [x]) mempty


data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  Flux Nothing _ <> f = f
  f <> Flux Nothing _ = f
  Flux (Just (x,y)) n <> Flux (Just (x',y')) n'
    | y == x'   = Flux (Just (x,y')) (n+n')
    | otherwise = Flux (Just (x,y')) (n+n'+1)

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0 
