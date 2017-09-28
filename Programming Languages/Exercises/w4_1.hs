-----------------------
module Ex5 where

import Data.List

main :: IO ()
main = undefined
-----------------------

-- Ex1: List functions with their complexities

{- a) Describe what the following functions do, and give their time complexities

nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub [y <- xs, x /= y]
O(n^2)


delete :: Eq a => a -> [a] -> [a]
delete y [] = []
delete y (x:xs) = if x == y 
      then xs 
      else x : delete y xs
O(n)


(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

O(n^2)

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ (ys \\ xs)
O(n^2)


intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, x `elem` ys]
O(mn)

-}

-- b) give an alternative version of nub, using filter
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

{-
[1,2,2,3,3,3,1,1] <- delete 1's
delete' 1 [1,2,2,3,3,3,1,1]
check 1 [] = 1(delete) == 1(fold r) --> []
[1,2,2,3,3,3,1] []
check 1 [] = 1(delete) == 1(fold r) --> []
[1,2,2,3,3,3] []
check 3 [] = 1 (delete) == 3(foldr) --> 3:[]
.
.
.
-}

-- c) give an alternative definition of delete, using foldr
delete' :: Eq a => a -> [a] -> [a]
delete' y xs = foldr check [] xs
  where
    check x ys
       | y == x = ys
       | otherwise = x : ys
        
-- d) Can delete be defined in terms of filter or a list comprehension?
deleteF :: Eq a => a -> [a] -> [a]
deleteF y [] = []
deleteF y xs = filter ( /= y) xs


{-
deleteF y = filter(/=)
-}

deleteLC :: Eq a => a -> [a] -> [a]
deleteLC y xs = [x | x <- xs, x /=y]



-- e) give an alternative definition of (\\), using explicit recursion
diff :: Eq a => [a] -> [a] -> [a]
diff xs []       = xs
diff xs (y : ys) = diff (delete y xs) ys

-- give an alternative definition of intersect, using filter
--intersect' :: Eq a => [a] -> [a] -> [a]
--intersect' xs ys = filter (elem ys) xs


-- 2 - Functions on trees

-- We define the functions:
data Tree a = Nil | Node a (Tree a) (Tree a)

-- a) - define the type of foldTree
-- foldTree :: (a -> b -> b -> b) b -> Tree a -> b
foldTree _ z Nil = z
foldTree f z (Node a l r) = f a (foldTree f z l) (foldTree f z r)


-- b) - tree functions based on foldTree

height :: Tree a -> Int
height = foldTree f 0
  where
    f _ l r =  1 + max l r 


size :: Tree a -> Int
size = foldTree f 0
  where
    f _ l r =  1 + l + r


treesum :: Num a => Tree a -> a
treesum = foldTree f 0
  where
    f a l r = a + l + r
    --f = valCurrent resLeft resRight = valCurrent + resLeft + resRight
flatten :: Tree a -> [a]
flatten = foldTree f []
  where
    f current resL resR = resL ++ [current] ++ resR    -- in-order
    --f current resL resR = current : resL ++ resR     -- pre-order
    --f current resL resR = resL ++ resR ++ [current]  -- post-order
    
    
-- 3) - Functor

-- a) - show that `fmap id h = id h = h` still holds, when `fmap = (.)`
{-
fmap = (.)
fmap id h x
= (id . h) x -- fmap = (.)
= id (h x )  --definition of (.)
= h x        --definition of id
-}
-- b) - show that `fmap (f . g) h = fmap f (fmap g h)`
{-
fmap ((f . g) h) x
= ((f . g) . h) x
= (f . g) (h x)         -- definiton of (.)
= f (g (h x))           -- definiton of (.)
= f ((g . h) x          -- definiton of (.)
= (f . (g . h)) x       -- definiton of (.)
= (fmap f (fmap g h)) x -- fmap = (.)
-}


-- 4) - Compare the following list comprehensions, and convert them to combinatory style


{-
(only continue if x is odd )
[ (x,y) | x <- [1..n], odd x, y <- [1..n] ] 
[ (x,y) | x <- [1..n], y <- [1..n], odd x ] 


-}

l1 :: Integral a => a -> [(a, a)]
l1 = concat 
   $ map (x\x -> zip (repeat x) [1..n]) 
   $ filter (odd) [1..n]
   
l2 :: Integral a => a -> [(a, a)]
l2 = filter (odd.fst)
    $concat
    $map(\x -> zip (repeat x) [1..n]) [1..n]


-- 5) - Transpose without list comprehensions
transpose' :: [[a]] -> [[a]]
transpose' = undefined


-- 6) - remdups --> [1,2,2,3,3,3,1,1] -> [1,2,3,1]
-- remdups ::
remdups = undefined
