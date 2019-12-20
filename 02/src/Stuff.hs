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
--group (x:xs) =
--    case group xs of
--       []        -> [[x]]
--       ([]:_)    -> undefined 
--       (y:ys):zs -> if x == y then
--                       (x:y:ys):zs
--                    else
--                      [x]:(y:ys):zs
                     

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x (y:ys) = case f x y of
                        GT  -> y : insertBy f x ys
                        _   -> x : y : ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f = foldr (insertBy f) [] 

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = 
    case groupBy eq xs of
      []         -> [[x]]
      ([]:_)     -> undefined 
      (y:ys):zs  -> if eq x y then
                       (x:y:ys):zs
                    else
                       [x]:(y:ys):zs

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)  

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x) 

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map fst . sortBy (compare `on` snd) . map (id &&& f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = map (map fst) . groupBy ((==) `on` snd) . map (id &&& f)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = groupOn f . sortOn f

  
