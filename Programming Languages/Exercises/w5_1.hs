module Ex6 where
main :: IO ()
main = undefined

-------------------


{- Identify redexes
1 + (2*3)


(1+2) * (2+3)


fst (1+2, 2+3)


(\x -> 1 + x) (2*3)


-}


-------------------------

{- Outermost vs Innermost evaluation for fst

Innermost
fst ((1+2), (2+3))
=...

Outermost
fst ((1+2), (2+3))
= ...

-}

--------------------------

{- Reduce cube (cube 3)
Innermost
cube (cube 3)
= ...

Outermost
cube (cube 3)
= ...

Outermost with sharing
cube (cube 3)
= ...

-}

------------------------

{- Definition of map
map f [] = []
map f (x:xs) = f x : map f xs
-}

{- Evaluation of map (Outermost with sharing)
map (2*) (map (1+) [1,2,3])
= ...
-}

-------------------------------

-- Strict map implementation
mapStrict :: (a -> b) -> [a] -> [b]
mapStrict f l = undefined

-------------------------------

-- Iterate

-- Natural numbers
nats :: [Int]
nats = iterate f 0
  where f = undefined

-- Even numbers
evens :: [Int]
evens = iterate f 0
  where
    f = undefined

-- Two-powers
twopows :: [Int]
twopows = iterate f 1
  where
    f = undefined

-- Factorials
factorials :: [(Int, Int)]
factorials = iterate f (0, 1)
  where
    f (a, b) = undefined

-- Fibs
fibs :: [(Int, Int)]
fibs = iterate f (0, 1)
  where
    f (a, b) = undefined

---------------------------

-- Infinite lists

pairs :: [[(Int, Int)]]
pairs = [ [ (x, y) | y <- [1..] ] | x <- [1..] ]

-- taketake
taketake :: Int -> [[a]] -> [[a]]
taketake n = undefined

-- diagonals
diags :: [[a]] -> [[a]]
diags xs = undefined

-------------------------

-- Full binary trees
data FTree a = Nil | Cons a (FTree (a, a)) deriving Show

-- Constructors level0-2
level0, level1, level2 :: FTree Int
level0 = undefined
level1 = undefined
level2 = undefined

-- levels (take)
levels :: Int -> FTree a -> FTree a
levels n t = undefined

-- Split
split :: FTree (a, a) -> (FTree a, FTree a)
split t = undefined

-- Left
left :: FTree a -> Maybe (FTree a)
left t = undefined

-- Right
right :: FTree a -> Maybe (FTree a)
right t = undefined

-- Join
join :: (FTree a, FTree a) -> FTree (a, a)
join (t1, t2) = undefined

-- GenTree
genTree :: Integer -> FTree Integer
genTree n = undefined
