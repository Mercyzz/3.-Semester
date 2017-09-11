module PolyList where

canonical :: (Num a,Eq a) => [a] -> [a]
canonical = undefined

deg :: [a] -> Int
deg = undefined

lead :: Num a => [a] -> a
lead = undefined

neg :: (Num a,Eq a) => [a] -> [a]
neg = undefined

add :: (Num a,Eq a) => [a] -> [a] -> [a]
add = undefined

sub :: (Num a,Eq a) => [a] -> [a] -> [a]
sub = undefined

addMany :: (Num a,Eq a) => [[a]] -> [a]
addMany = undefined

mulconstant :: (Num a,Eq a) => a -> [a] -> [a]
mulconstant = undefined

mulpower :: (Num a,Eq a) => Int -> [a] -> [a]
mulpower = undefined

diff :: (Num a,Eq a) => [a] -> [a]
diff = undefined

int :: (Fractional a,Eq a) => [a] -> [a]
int = undefined

mul :: (Num a,Eq a) => [a] -> [a] -> [a]
mul = undefined

eval :: (Num a,Eq a) => [a] -> a -> a
eval = undefined

compose :: (Num a,Eq a) => [a] -> [a] -> [a]
compose = undefined

polydiv :: (Fractional a,Eq a) => [a] -> ([a], [a]) -> ([a], [a])
polydiv = undefined
