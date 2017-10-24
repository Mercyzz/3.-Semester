import Data.List
main :: IO()
main = undefined

reverseLinear :: [a] -> [a]
reverseLinear l = reverse' l []
 where
   reverse' []acc = acc
   reverse' (x:xs) acc = reverse' xs (x:acc)


