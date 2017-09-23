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
null Nil = True
null _ = False

empty :: Dict k a 
empty = Nil

singleton :: (k, a) -> Dict k a
singleton kv = Node 1 kv Nil Nil

height :: Dict k a -> Height
height Nil = 0
height (Node h _ _ _) = h

-- Ex2) Smart constructor

node :: (k,a) -> Dict k a -> Dict k a -> Dict k a
node kv l r = Node (1+max (height l) (height r)) kv l r

-- Ex3) Insertion
--a = singleton (10, 5)
--b = singleton (20, 20)
--c = node (15,90) a b

insert :: Ord k => (k, a) -> Dict k a -> Dict k a
insert kv Nil = singleton kv
insert kv@(k,_) (Node _ kv'@(k',_) l r)
     | k == k' = node kv l r
     | k < k' = node kv' (insert kv l) r
     | k > k' = node kv' l (insert kv r)

--- Ex4) Lookup

lookup :: Ord k => k -> Dict k a -> Maybe a
lookup _ Nil = Nothing
lookup k (Node _ (k', v) l r) 
     | k == k' = Just v
     | k < k' = lookup k l
     | k > k' = lookup k r


--rangeLookup :: Ord k => k -> k -> Dict k a -> [(k,a)] 
--rangeLookup 
 
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