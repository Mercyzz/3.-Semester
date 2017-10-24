module Dict where
import Prelude hiding (null,lookup)

type Height     = Int
type Balance    = Int
data Dict k a   = Node Height (k,a) (Dict k a) (Dict k a) | Nil deriving (Eq)

instance (Show k, Show a) => Show (Dict k a) where
  show Nil = "Nil"
  show (Node h (k,x) l r) = "(key="++ (show k) ++ ", value=" ++ (show x) ++ ", height=" ++ (show h) ++ ")\n" 
        ++ "├──" ++ (drop 3 $ unlines $ map ("│   " ++) $ lines $ show l)
        ++ "└──" ++ (drop 3 $ unlines $ map ("    " ++) $ lines $ show r)

-- Ex 1) Simple functions

null :: Dict k a -> Bool
null = undefined

empty :: Dict k a 
empty = undefined

singleton :: (k, a) -> Dict k a
singleton = undefined

height :: Dict k a -> Height
height = undefined

-- Ex2) Smart constructor

node :: (k,a) -> Dict k a -> Dict k a -> Dict k a
node = undefined

-- Ex3) Insertion

insert :: Ord k => (k, a) -> Dict k a -> Dict k a
insert = undefined
 
--- Ex4) Lookup

lookup :: Ord k => k -> Dict k a -> Maybe a
lookup = undefined

rangeLookup :: Ord k => k -> k -> Dict k a -> [(k,a)] 
rangeLookup = undefined
 
-- Ex 5) Load values from list

fromList :: Ord k => [(k, a)] -> Dict k a
fromList = undefined

-- Ex 6) Output values to list
flatten :: Dict k a -> [(k,a)]
flatten = undefined

-- Ex 7) Deletion

leftmost :: Dict k a -> Maybe (k,a)
leftmost = undefined


delete :: Ord k => k -> Dict k a -> Dict k a
delete = undefined

-- Ex 8) Balance property

bal :: Dict k a -> Balance
bal = undefined

hasValidBalance :: Dict k a -> Bool
hasValidBalance = undefined

-- Ex 9) Rotation

rotate :: Ord k => Dict k a -> Dict k a
rotate = undefined 