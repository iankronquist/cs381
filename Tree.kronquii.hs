module Tree where


--
-- * Part 2: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int
-- Base case
rightmost (Leaf i) = i
-- Recursion
rightmost (Node _ _ r) = rightmost r


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt :: Tree -> Int
maxInt (Node n l r) = max (max n $ maxInt l) (max n $ maxInt r)
maxInt (Leaf l) = l


-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree -> Int
minInt (Node n l r) = min (min n $ minInt l) (min n $ minInt r)
minInt (Leaf l) = l


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--

sumInts :: Tree -> Int
sumInts (Node t l r) = t + (sumInts l) + (sumInts r)
sumInts (Leaf i) = i


-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--   
preorder :: Tree -> [Int]
preorder (Node t l r) = [t] ++ (preorder l) ++ (preorder r)
preorder (Leaf l) = [l]


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   
inorder :: Tree -> [Int]
inorder (Node t l r) = (inorder l) ++ [t] ++ (inorder r)
inorder (Leaf l) = [l]

-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--   
isBST :: Tree -> Bool
isBST (Node i l r) = (i >= getVal l) && (i < getVal r) && isBST l && isBST r
isBST (Leaf l) = True

getVal :: Tree -> Int
getVal (Node i _ _) = i
getVal (Leaf l) = l


-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--   
inBST :: Int -> Tree -> Bool
inBST i (Node n l r)
  | n == i = True
  | n > i = inBST i l
  | n < i = inBST i r
inBST i (Leaf n) = n == i
