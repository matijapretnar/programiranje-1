-- AVL trees

{- resources:
   - https://visualgo.net/bst  nice visualisation of all operations, recommended!

   the English wiki article on AVL trees is very poorly written; the article on
   rotations is worthwhile
   - https://de.wikipedia.org/wiki/AVL-Baum  brush up on your German! or use
     google translate
   - https://en.wikipedia.org/wiki/Tree_rotation
-}


data AVL a = Leaf | Node Int (AVL a) a (AVL a) deriving Show

empty :: AVL a
empty = undefined

height :: AVL a -> Int
height = undefined

-- [avl left a right] is a smart constructor that fills in the height of the
-- new tree. The height of [left] and [right] may differ by at most 1
avl :: AVL a -> a -> AVL a -> AVL a
avl = undefined

-- check if an element appears in a tree
element :: Ord a => a -> AVL a -> Bool
element = undefined

-- [skew t] computes how "disbalanced" [t] is, ie. how its subtrees compare in
-- height. A search tree is an AVL tree is this factor is never greater than 1.
skew :: AVL a -> Int
skew = undefined

-- Perform a right rotation
rotR :: AVL a -> AVL a
rotR = undefined

-- Perform a left rotation
rotL :: AVL a -> AVL a
rotL = undefined

-- [rebalance t] performs the rotations necessary to re-balance an almost-AVL
-- tree which is skewed by at most 2.
rebalance :: AVL a -> AVL a
rebalance = undefined

-- [add t a] inserts an element into an AVL tree, ensuring that the invariants
-- are respected.
add :: (Ord a) => AVL a -> a -> AVL a
add = undefined

-- With [add] implemented, we can easily construct an AVL tree from a list
fromList :: (Ord a) => [a] -> AVL a
fromList = undefined

-- [minAVL t] finds the minimum element of [t]
minAVL :: (Ord a) => AVL a -> Maybe a
minAVL = undefined

-- [delete t a] returns an AVL tree [t] with [a] removed
delete :: Ord a => AVL a -> a -> AVL a
delete = undefined
