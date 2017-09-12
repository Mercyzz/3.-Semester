
{-
somefunc x = func x where funcy = y + 2

cs :: [Int]-> [Int]
cs [] = []
cs(c:xs)=cs' x xs
cs':: Int -> [Int] -> [Int]
cs' x [] = [x]
cs' prevSum (y:ys) = (prevSum + y): cs' (y+prevSum) ys
-}

--Exercise 1
notImpl :: Bool ->Bool
notImpl True = False
notImpl False = True

xorImpl :: Bool -> Bool -> Bool
xorImpl True False = True
xorImpl True True = False
xorImpl False True =  True
xorImpl False False = False

nandImpl :: Bool -> Bool -> Bool
nandImpl True False = True
nandImpl True True = False
nandImpl False True = True
nandImpl False False = True

orImpl :: Bool -> Bool -> Bool
orImpl True False = True
orImpl True True = True
orImpl False True = True
orImpl False False = False

orImpl1 :: Bool -> Bool -> Bool
orImpl1 False a = a
orImpl1 _ _ = True

xorImpl1 :: Bool -> Bool -> Bool
xorImpl1 False False = False
xorImpl1 True True = False
xorImpl1 _ _ = True


nandImpl1 :: Bool -> Bool -> Bool
nandImpl1 True True = False
nandImpl1 _ _ = True


--Exercise 2
eeny :: Integer -> String
eeny x = if x `mod` 2 == 0
     then "eeny"
     else "meeny"
     
--Exercise 3
fizzbuzz :: Integer -> String
fizzbuzz x | x `mod`3 == 0 && x`mod`5 == 0 = "FizzBuzz" 
           | x `mod`3 == 0 = "Fizz" 
           | x `mod`5 == 0 = "Buzz"
           | otherwise = "Not FizzBuzz"

--Recursive functions on integers 
--Exercise 1
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo(n-1)

--Exercise 2
binomial :: Integer -> Integer -> Integer
binomial n k | k == 0 || k == n = 1 
             | k < 0 || k > n = 0 
             |1<=k && k <= (n-1) = binomial (n-1) k + binomial (n-1) (k-1)

--Exercise 3
power :: Integer -> Integer -> Integer
power n k | k == 0 = 1
          | k > 0 = n * (power n (k-1))
          
--Exercise 4
ilog2 :: Integer -> Integer
ilog2 1 = 0
ilog2 x = 1 + ilog2(x`div`2)

--Recursive functions on lists
--Exercise 1
dropZeros :: [Int]->[Int]
dropZeros [] = []
dropZeros (y:ys) | y == 0 = dropZeros(ys)| otherwise =  y : dropZeros(ys)

--Exercise 2
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x++ flatten xs

--Exercise 3
twiceAll :: [a] -> [a]
twiceAll [] = []
twiceAll (x:xs) =   x : (x : (twiceAll xs))

--Exercise 4
alternate :: [Int] -> [Int]
alternate [] = []
alternate [x] = [x]
alternate (x:y:xs) = x : -y : (alternate xs)

--Exercise 5
--replicateAll :: Int -> [a] -> [a]
--replicateAll n (x:xs) |n <= 0 = [] | otherwise = (replicateAll n x) ++ replicateAll n xs
--replicateAll n (x:xs) | _ [] = [] | n == 0  || n == 1 = x:xs | n < 0 = error "Error" | otherwise = n : replicateAll (n-1) x:xs

--Exercise 6
cumulativeSum :: [Int ] -> [Int ]
cumulativeSum [] = []
cumulativeSum [x] = [x]
cumulativeSum (x:y:ys) = x: (cumulativeSum $ (x+y) : ys)

--(cumsum $ (x+y) : ys)

--INSERTON SORT
insert :: Ord a => a-> [a] ->[a]
insert x [] = [x]
insert x (y:ys)| x<= y = x:y:ys | otherwise = y:insert x ys

isort :: Ord a => [a]->[a]
isort [] = []
isort (x:xs) = insert x (isort xs)

cartesian xs ys = [(x,y)|x<-xs,y<-ys]
--result: [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
cartesian' xs ys = [(x,y)|y<-ys,x<-xs]
--result: [(1,'a'),(2,'a'),(3,'a'),(1,'b'),(2,'b'),(3,'b'),(1,'c'),(2,'c'),(3,'c')]
