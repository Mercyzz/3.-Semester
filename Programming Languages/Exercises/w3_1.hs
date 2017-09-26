main :: IO ()
main = undefined


-- 1: Under which conditions does [x|x←xs,y←ys]≡[x|y←ys,x←xs] hold ?


-- 2: define functions
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

repeat :: a -> Tree a
repeat x = undefined

take :: Int -> Tree a -> Tree a
take n _ = undefined

replicate :: Int -> a -> Tree a
replicate n = undefined


-- 3 - Traversal

data Direction = L | R deriving Show

elementAt :: [Direction] -> Tree a -> Maybe a
elementAt _ _ = undefined

modifyAt :: (a -> a) -> [Direction] -> Tree a -> Tree a
modifyAt _ _ _ = undefined


-- 4 - toTree

toTree :: [a] -> Tree a
toTree _ = undefined



-- 5 - Interpreted language

data Expression =
  Var String -- Variable
  | Val Int  -- Integer literal
  | Op Expression Bop Expression -- Operation

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Gt | Ge | Lt | Le | Eql
  deriving (Show, Eq)

type State = String -> Maybe Int


-- Things to note: If Exp is Var, consult database(State) for its value.
-- Binary operators can only be used on two Ints (Right)
evalE :: State -> Expression -> Either String Int
evalE _ _ = undefined





-- Example states:

myState :: State
myState x = lookup x [
  ("Klaus Meer", error "Counted to infinity twice"),
  ("BoomBox", 5*10^6)]


anotherState :: State
anotherState "Bjarne Toft" = Just 42
anotherState "test" = Just 1
anotherState _      = Nothing
