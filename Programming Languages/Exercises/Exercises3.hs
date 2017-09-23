import Data.List
main :: IO()
main = undefined

--Exercise 1
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x


--Exercise 2
zipIdx :: [a] -> [(a, Int)]
zipIdx [] = []
zipIdx list = zipIdx' 0 list
   where 
     zipIdx' _ [] = []
     zipIdx' n (x:xs) = (x, n) : zipIdx'(n+1) xs

--Exercise 3
setIdx :: [a] -> Int -> a -> [a]
setIdx [] _ _ = []
setIdx (_:xs) 0 e = e : xs
setIdx (x:xs) n e = x : setIdx xs (n-1) e 


--Exercise 4
modIdx :: [a] -> Int -> (a -> a) -> [a]
modIdx[] _ _ = []
modIdx (x:xs) 0 f = f x : xs
modIdx (x:xs) n f = x : modIdx xs (n-1) f

--Exercise 5
--Part 1
nubEq :: Eq a => [a] -> [a]
nubEq []     = []
nubEq (x:xs) = x: nubEq [y | y <- xs
                           ,x /=y]
--Part 2                   
nubOrd :: Ord a => [a] -> [a]
nuOrd [] = []
nubOrd list = nubOrd' (sort list)
  where 
    nubOrd' []       = []
    nubOrd' [x]      = [x]
    nubOrd' (x:y:xs) 
            | x == y = nubOrd'(y: xs) 
            | otherwise = x : nubOrd' (y:xs)             
--Exercise 6
--[1,2,3,1] -> [(1,2),(2,1),(3,1)]
elemCountsEQ :: Eq a => [a] -> [(a, Int)]
elemCountsEQ [] = []
elemCountsEQ l@(x:_) = (x, length getEq) : elemCountsEQ getNEq
  where 
     getEq = [y | y<- l, y == x]
     getNEq = [y | y<- l, y /= x]


elemCountsOrd :: Ord a => [a] -> [(a,Int)]
elemCountsOrd [] = []
elemCountsOrd list = elemCountsOrd' (first, l) rest
   where
   (first:rest) = sort l
   elemCountsOrd' (e, occ) [] = [(e,occ)]
   elemCountsOrd' (e, occ) (x:xs)
                                 | e == x = elemCountsOrd' (e, occ+1) xs
                                 | otherwise = (e,occ) : elemCountsOrd' (x,1) xs
 
--Exercise 7
fromElemCounts :: [(a,Int)] -> [a]
fromElemCounts [] = []
fromElemCounts ((x,n) : xs) = replicate n x ++ fromElemCounts xs

--Exercise 8
type Dist a = [(a, Double)]
-- a
uniformly :: [a] -> Dist a
uniformly l = zip l [l/(length l)..]


-- b


-- c