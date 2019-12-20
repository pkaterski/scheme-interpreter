module StuffNonEmpty
  ( NonEmpty(..)
  , mapNonEmpty
  , groupNonEmpty
  , groupByNonEmpty
  , groupOnNonEmpty
  , classifyOnNonEmpty
  ) where

import Stuff (sortOn, sortBy, on, (&&&))

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupOnNonEmpty id 

data NonEmpty a = a :| [a]
  deriving (Show, Eq, Ord)
infixr 4 :|

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (x:|xs) = f x :| map f xs  

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] = []
groupByNonEmpty f (x:xs) = 
    case groupByNonEmpty f xs of
       []          -> [x :| []] 
       (y:|ys):zs  -> if f x y then
                         (x:|(y:ys)):zs
                      else
                         (x:|[]):(y:|ys):zs

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f = groupByNonEmpty ((==) `on` f) 

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f = groupOnNonEmpty f . sortOn f 
