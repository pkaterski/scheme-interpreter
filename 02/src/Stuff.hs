module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

group :: Eq a => [a] -> [[a]]
group = groupOn id
--group [] = []
--group [x] = [[x]]
--group [x,y] = if x == y then [[x,y]] else [[x],[y]]
--group (x:y:xs) = if x /= y then
--                  [x]:group (y:xs)
--                 else
--                  case group xs of
--                    []        -> [[x,y]]
--                    ([]:_)    -> undefined 
--                    (z:zs):ts -> if y == z then
--                                  (x:y:z:zs):ts
--                                 else
--                                  [x,y]:(z:zs):ts
                     

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x (y:ys) = case f x y of
                        GT  -> y : insertBy f x ys
                        _   -> x : y : ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = insertBy f x $ sortBy f xs 

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy _ [x] = [[x]]
groupBy f [x,y] = if f x y then [[x,y]] else [[x],[y]]
groupBy f (x:y:xs) = if f x y then
                      case groupBy f xs of
                        []     -> [[x,y]]
                        ([]:_) -> undefined 
                        zs:ts  -> if f y (head zs) then
                                   (x:y:zs):ts
                                  else
                                   [x,y]:zs:ts
                     else
                      [x]:groupBy f (y:xs)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)  

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x) 

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f) 

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)  

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = (groupOn f) . (sortOn f)

  