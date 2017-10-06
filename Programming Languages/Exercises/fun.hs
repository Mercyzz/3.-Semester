{-
1 - shift
====

Make a function which shifts a list n positions.

Examples:
    shift 1    [1,2,3,4] == [4,1,2,3]
    shift (-1) [1,2,3,4] == [2,3,4,1]

-}


shift :: Int -> [a] -> [a]
shift 0 xs = xs
shift _ [] = []
shift n xs = snd splitted' ++ fst splitted' where
  splitted'= splitAt(mod (lengthXs-n) lengthXs) xs
  lengthXs = length xs

{-
2 - inject
====

Make a function which injects an element at a specific position of a list

Examples:
    inject 0 'x' "abc" == "xabc"
    inject 1 'x' "abc" == "axbc"
    inject 2 'x' "abc" == "abxc"
 

-}
inject :: Int -> a -> [a] -> [a]
inject _ a [] = [a]
inject i x xs = fst splitted' ++ x:snd splitted' where
  splitted'= splitAt i xs

{-
2 - innerProd
====

Make a function which for two lists computes the inner product of the lists, i.e.

  innerProd [x1,x2,x3] [y1,y2,y3] == x1*y1 + x2*y2 + x3*y3

-}
innerProd :: (Num a) => [a] -> [a] -> a
innerProd xs ys = foldr (\t s -> s + fst t * snd t) 0 (zip xs ys)

{-
3 - outerProd
====

Make a function which for two lists computes the outer product of the lists, i.e.

  outerProd [x1,x2,x3] [y1,y2,y3,y4] == [[x1*y1, x1*y2, x1*y3, x1*y4],
                                         [x2*y1, x2*y2, x2*y3, x2*y4],
                                         [x3*y1, x3*y2, x3*y3, x3*y4]]
-}

outerProd :: (Num a) => [a] -> [a] -> [[a]]
outerProd xs ys = [[x*y | y <- ys] | x <- xs]

{-
4 - chop
====

Make a function which chops a list into lists of equal length, except for the last list i.e.
  
  chop 3 [1,2,3,4,5,6,7,8,9,10] == [[1,2,3], [4,5,6], [7,8,9], [10]]


-}
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop 0 xs = [xs]
chop n xs = fst splitted':chop n (snd splitted') where
  splitted'= splitAt n xs

{-
5 - sumLists
====

Make a function which sums a list of lists i.e.

  sumLists [[1,2,3], [4,5,6], [7,8,9], [10]] == [1+4+7+10, 2+5+8, 3+6+9]

-}
sumLists :: (Num a) =>  [[a]] -> [a]
sumLists [] = []
sumLists xxs = sum (map head xxs) : sumLists (filter (not.null) (map tail xxs))


{-
6 - digits 
====

Make a function which returns the digits of a number in reverse order, i.e.

Example:

  digits 1337 == [7,3,3,1]

-}
digits :: (Integral a) => a -> [a]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)


{-
7 - insertDecr
====

Make a function which inserts an element in the correct position of a decreasing list

Example:
    insertDecr 5 [8,6,3] == [8,6,5,3]
-}
insertDecr :: (Ord a) => a -> [a] -> [a]
insertDecr x [] = [x]
insertDecr x xs = takeWhile(\x' -> x'>x) xs ++ x:dropWhile(\x' -> x'>x) xs

{-
8 - sortedDecr
====

Makes a function which detects if a list is sorted in decreasing order
-}
sortedDecr :: (Ord a) => [a] -> Bool
sortedDecr  [] = True
sortedDecr [x] = True
sortedDecr (x1:x2:xs) 
    | x1 >= x2 = sortedDecr (x2:xs)
    | otherwise = False