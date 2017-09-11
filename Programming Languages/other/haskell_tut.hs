import Data.List
import System.IO

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

times4 :: Int -> Int
times4 x = x * 4

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)

getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

db1To10 = map(\x -> x * 2) [1..10]

doubleEvenNumber y = if (y `mod` 2 /= 0) then y else y *2

getClass :: Int -> String
getClass n = case n of
    5 -> "go to kindergarten"
    6 -> "Go to elementary school" 
    _ -> "Go away"
    
data BaseBallPlayer = Pitcher 
                     | Catcher 
                     | Infielder 
                     | Outfield 
                deriving Show

barryBonds :: BaseBallPlayer -> Bool
barryBonds Outfield = True

barryInOf = print(barryBonds Outfield)

data Customer = Customer String String Double
   deriving Show
   
tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double

getBalance (Customer _ _ b) = b


data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Rock Beats Scissors"
shoot Scissors Paper = "Scissors Beat Paper"
shoot Scissors Rock = "Scissors Loses to Rock"
shoot Paper Scissors = "Paper Loses to Scissors"
shoot Rock Paper = "Rock Loses to Paper"
shoot _ _ = "Error"

data Shape =  Circle Float Float Float | Rectangle Float Float Float Float

area :: Shape -> Float

area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)

sumValue = putStrLn (show ( 1 + 2))

sumValue2 = putStrLn . show $1 +2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100

data Employee = Employee {name :: String, position :: String, idNum :: Int} deriving (Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001}

isSamPam = samSmith == pamMarx

samSmithData = show samSmith

data ShirtSize = S | M | L
instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False
    
instance Show ShirtSize where
   show S = "Small"
   show M = "Medium"
   show L = "Large"
   
smallAvail = S `elem` [S, M, L]

theSize = show S

class MyEq a where 
  areEqual :: a -> a -> Bool


instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False
  
newSize = areEqual M M

sayHello = do
  putStrLn "What's your name"
  name <- getLine
  putStrLn $ "Hello " ++ name
  
writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random line of text")
  hPutStrLn theFile ("Hej")
  hClose theFile
  
readFromFile = do
  theFile2 <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile2
  putStr contents
  hClose theFile2

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

fib300 = fib !! 300



  