module Dict where
import Prelude hiding (null,lookup)
import Data.Maybe(fromJust)
import Debug.Trace

type Height     = Int
type Balance    = Int
data Dict k a   = Node Height (k,a) (Dict k a) (Dict k a) | Nil deriving (Eq)

instance (Show k, Show a) => Show (Dict k a) where
  show Nil = "Nil"
  show (Node h (k,x) l r) = "(key="++ show k ++ ", value=" ++ show x ++ ", height=" ++ show h ++ ")\n"
        ++ "├──" ++ drop 3 (unlines $ map ("│   " ++) $ lines $ show l)
        ++ "└──" ++ drop 3 (unlines $ map ("    " ++) $ lines $ show r)

-- Ex 1) Simple functions

null :: Dict k a -> Bool
null Nil = True
null _   = False

empty :: Dict k a
empty = Nil

singleton :: (k, a) -> Dict k a
singleton v = Node 1 v Nil Nil

height :: Dict k a -> Height
height Nil            = 0
height (Node h _ _ _) = h

-- Ex2) Smart constructor

node :: (k,a) -> Dict k a -> Dict k a -> Dict k a
node val l r = Node (1 + max (height l) (height r)) val l r

-- Ex3) Insertion

insert :: Ord k => (k, a) -> Dict k a -> Dict k a
insert val Nil = node val Nil Nil
insert (k, a) (Node _ kv@(k', _) l r)
  | k == k'   = rotate $ node (k, a) l r
  | k <  k'   = rotate $ node kv (insert (k, a) l) r
  | otherwise = rotate $ node kv l (insert (k, a) r)

-- Ex4) Lookup

lookup :: Ord k => k -> Dict k a -> Maybe a
lookup _ Nil  = Nothing
lookup k (Node _ (k', a) l r)
  | k == k'   = Just a
  | k < k'    = lookup k l
  | otherwise = lookup k r

rangeLookup :: Ord k => k -> k -> Dict k a -> [(k,a)]
rangeLookup _ _ Nil = []
rangeLookup kl kh (Node _ (k, a) l r)
  | kh < k = rangeLookup kl kh l
  | k < kl = rangeLookup kl kh r
  | inRange = (k, a) : rangeLookup kl kh l ++ rangeLookup kl kh r
    where
      inRange = kl <= k && k <= kh

-- Ex 5) Load values from list
-- [(6, 0), (5, 0), (4, 0), (3, 0), (2, 0), (1, 0), (0, 0)]
fromList :: Ord k => [(k, a)] -> Dict k a
fromList = foldl (flip insert) empty

-- Ex 6) Output values to list
flatten :: Dict k a -> [(k,a)]
flatten Nil             = []
flatten (Node _ kv l r) = flatten l ++ kv : flatten r

-- Ex 7) Deletion

leftmost :: Dict k a -> Maybe (k,a)
leftmost Nil = Nothing
leftmost (Node _ (k, a) l _) = case leftmost l of
  Nothing -> Just (k, a)
  Just kv -> Just kv

-- There is probably a prettier solution.
delete :: Ord k => k -> Dict k a -> Dict k a
delete _ Nil = Nil
delete k nod@(Node _ kv@(k', _) l Nil)
  | k == k'   = rotate l
  | k >  k'   = rotate nod
  | otherwise = rotate $ node kv (delete k l) Nil
delete k nod@(Node _ kv@(k', _) Nil r)
  | k == k'   = rotate r
  | k <  k'   = rotate nod
  | otherwise = rotate $ node kv Nil (delete k r)
delete k (Node _ kv@(k', _) l r)
  | k < k'  = rotate $ node kv (delete k l) r
  | k > k'  = rotate $ node kv l (delete k r)
  | k == k' = rotate $ node
    (fromJust (leftmost r))
    l
    (delete (fst (fromJust (leftmost r))) r)

-- Ex 8) Balance property

bal :: Dict k a -> Balance
bal Nil            = 0
bal (Node _ _ l r) = height r - height l

hasValidBalance :: Dict k a -> Bool
hasValidBalance Nil = True
hasValidBalance this@(Node _ _ l r) = valid this
                                      && hasValidBalance l
                                      && hasValidBalance r
  where
    valid x = abs (bal x) <= 1

-- We can redefine foldTree from exercise sheet 5, for Dict.
foldDict :: (Height -> (k, a) -> b -> b -> b) -> b -> Dict k a -> b
foldDict _ z Nil = z
foldDict f z (Node h kv l r) = f h kv (foldDict f z l) (foldDict f z r)

-- Ex 9) Rotation
-- not the nicest implementation.
rotate :: Ord k => Dict k a -> Dict k a
rotate Nil    = Nil
rotate nod = foldDict rotate' Nil nod
    where
      rotate' _ kv Nil Nil   = singleton kv
      rotate' _ kv resL resR = case bal $ node kv resL resR of
        (2)  -> case (resR, bal resR) of
          (Node _ kvr (Node _ kvrl rll rlr) rr, -1) -> node kvrl
                                                            (node kv resL rll)
                                                            (node kvr rlr rr)
          (Node _ kvr rl rr, _)                     -> node kvr
                                                            (node kv resL rl)
                                                            rr
        (-2) -> case (resL, bal resL) of
          (Node _ kvl ll (Node _ kvlr lrl lrr), 1)  -> node kvlr
                                                            (node kvl ll lrl)
                                                            (node kv lrr resR)
          (Node _ kvl ll lr, _)                     -> node kvl
                                                            ll
                                                            (node kv lr resR)
        _ -> node kv resL resR
