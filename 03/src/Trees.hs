{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)
import Data.Bifunctor (second) --hlint kaza taka da napravq

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Node x l r == Node x' l' r' =
    x == x' && l == l' && r == r'
  _ == _ = False 

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty 
insertOrdered x (Node y l r)
  | x > y     = Node y l (insertOrdered x r)
  | otherwise = Node y (insertOrdered x l) r

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty

isBST :: Ord a => Tree a -> Bool
isBST = between Bot Top 

-- idea for implementing isBST - delete if you don't want it
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between low up Empty = low <= up 
between low up (Node x l r) = 
     between low (Val x) l 
  && between (Val x)  up r

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST x (Node y l r)
  | x > y = findBST x r
  | x < y = findBST x l
  | otherwise = True 

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r) 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = fmap

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node x l r) = foldTree l <> x <> foldTree r


foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = foldTree . mapTree f 

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum 

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = getAll . foldMapTree (All . p) 

treeToList :: Tree a -> [a]
treeToList = foldMapTree (:[]) 

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x = getAny . foldMapTree (Any . (==x)) 

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred f = getFirst . foldMapTree (First . onMaybe  f) 

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = foldMapTree (\x -> [x | p x]) 

-- no need, maybe is already a monad :D
ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree _ Empty = Just Empty
validateTree p (Node x l r) = do
  l' <- validateTree p l
  x' <- p x
  r' <- validateTree p r
  return (Node x' l' r') 

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch _ Empty = Nothing
fetch [] (Node x _ _) = Just x
fetch (x:xs) (Node _ l r) = case x of
  L -> fetch xs l
  R -> fetch xs r 

paths :: Tree a -> [(a, [Direction])]
paths Empty = []
paths (Node x l r) = 
      map (second ((:) L)) (paths l)
  ++  [(x,[])] 
  ++  map (second ((:) R)) (paths r)
