{-
1 - map, filter and concat using recursion
====

Implement map, filter and concat using recursion on lists
-}
mapR :: (a -> b) -> [a] -> [b]
mapR f xs = undefined

filterR :: (a -> Bool) -> [a] -> [a]
filterR f xs = undefined

concatR :: [[a]] -> [a]
concatR xss = undefined

{-
2 - maximum and minimum using recursion
=====

Using recursion, implement functions which finds the minimum and maximum of a list.
-}
maximumR :: (Ord a) => [a] -> a
maximumR xs = undefined

minimumR :: (Ord a) => [a] -> a
minimumR xs = undefined

{-
3 - Implement map, filter and concat using list comprehensions
====

-}
mapLC :: (a -> b) -> [a] -> [b]
mapLC f xs = undefined

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f xs = undefined

concatLC :: [[a]] -> [a]
concatLC xss = undefined


{-
4 - Implement the functions above using foldr
====

-}

maximumF :: (Ord a) => [a] -> a
maximumF xs = foldr op e xs
  where 
    e        = undefined
    op x y   = undefined


minimumF :: (Ord a) => [a] -> a
minimumF xs = foldr op e xs
  where 
    e        = undefined
    op x y   = undefined


mapF :: (a -> b) -> [a] -> [b]
mapF f xs = foldr op e xs
  where 
    e        = undefined
    op x xs  = undefined
    
filterF :: (a -> Bool) -> [a] -> [a]
filterF f xs = foldr op e xs
  where 
    e        = undefined
    op x xs  = undefined

concatF :: [[a]] -> [a]
concatF xss = foldr op e xss
  where 
    e        = undefined
    op x xs  = undefined