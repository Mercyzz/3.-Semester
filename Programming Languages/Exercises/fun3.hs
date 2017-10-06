{- 
You are given the algebraic data type
-}
data Tree v = Node v [Tree v] deriving Show
{-
which represents non-empty trees with an arbitrary number of children at each level.

v can be interpreted as the value at each node,

To help you with the exercise, we provide a sample tree:
-}
testTree :: Tree Int
testTree = Node 3 [ Node 4
                  [ Node 5 []
                  , Node 6 []
                  , Node 7 []
                  ]
                , Node 9
                    [ Node 10 []
                    ]
                ]
{-
1) 
Provide a function which turns a value v into the a node with the value v and no children.
-}
toTree :: v -> Tree v
toTree v = Node v[]

{-
2) 
Provide a function which extracts the value at the root node
-}
rootValue :: Tree v -> v
rootValue (Node v _)= v


{-
3) 
Provide a function which checks whether a node is a leaf node. A node is a leaf if it does not have any children.
-}

isLeaf :: Tree v -> Bool
isLeaf (Node v []) = True
isLeaf (Node v a) = False


{-
4)
Provide a function which returns the leftmost path in a tree.
Example: leftmostPath testTree == [3,4,5]
-}
leftmostPath :: Tree v -> [v]
leftmostPath (Node v [])= [v]
leftmostPath (Node v a) = v : leftmostPath(head a)

{-
5)
Provide a function which returns all the node values in the tree

Example:
  valuesInOrder testTree == [3,4,5,6,7,9,10]

-}
valuesInOrder :: Tree v -> [v]
valuesInOrder (Node v a) = v : concat [valuesInOrder v]


{-
6)
Provide a function which returns all paths from the root to a leaf (the number of paths equals the number of leafs)

Examples:
  paths (toTree 5) == [[5]]
  paths testTree == [[3,4,5],[3,4,6],[3,4,7],[3,9,10]]
-}
paths :: Tree v -> [[v]]
paths = undefined



{-
7)
Provide a fold function for the Tree data type

-}
foldTree :: (v -> [a] -> a) -> Tree v -> a
foldTree = undefined



{-
8)
Use your foldTree function to compute the sum of all nodes in the tree.

Example: 
  sumTree testTree  == 44
  sum (valuesInOrder testTree ) == 44 (kontrol)

-}
sumTree :: Num v => Tree v -> v
sumTree = undefined


{-

EXTRA EXERCISES:
If you are in lack of time, please continue with the next exercise sheet.


9)
Provide a function which returns all values appearing at each level.

Example:
  valuesByLevel [testTree] = [[3],[4,9],[5,6,7,10]]
  
-}

valuesByLevel :: [Tree v] -> [[v]]
valuesByLevel = undefined


{-
10)
Provide an unfold function which generates a tree.

unfoldTree testFunc 3 == testTree

-}

unfoldTree :: (a -> (v, [a])) -> a -> Tree v
unfoldTree = undefined



testFunc 3 = (3,[4,9])
testFunc 4 = (4,[5,6,7])
testFunc 9 = (9,[10])
testFunc x = (x,[])
