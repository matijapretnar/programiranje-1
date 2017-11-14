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

-- here's a drawing function, which, combined with [putStrLn], lets you visualise a tree
data S = L | R
draw :: Show a => AVL a -> String
draw t = "\n" ++ draw' Nothing t 0 ++ "\n"
  where
    draw' _ Leaf _ = []
    draw' dir (Node _ l v r) d =
      draw' (Just R) r (d+1) ++ node dir ++ draw' (Just L) l (d+1)
      where
        node dir' = padding d ++
          case dir' of
            Nothing -> ""
            Just L -> "\\- "
            Just R -> "/- "
          ++ show v ++ "\n"
        padding n = replicate (n*4) ' '


empty :: AVL a
empty = Leaf

height :: AVL a -> Int
height Leaf = 0
height (Node h _ _ _) = h

-- [avl left a right] is a smart constructor that fills in the height of the
-- new tree. The height of [left] and [right] may differ by at most 1
avl :: AVL a -> a -> AVL a -> AVL a
avl l a r = Node h l a r where h = 1 + max (height l) (height r)

-- check if an element appears in a tree
element :: Ord a => a -> AVL a -> Bool
element _ Leaf = False
element x (Node _ l a r) = case compare x a of
  LT -> element x l
  EQ -> True
  GT -> element x r

-- [skew t] computes how "disbalanced" [t] is, ie. how its subtrees compare in
-- height. A search tree is an AVL tree is this factor is never greater than 1.
skew :: AVL a -> Int
skew Leaf = 0
skew (Node _ l _ r) = height l - height r

-- Perform a right rotation
rotR :: AVL a -> AVL a
rotR (Node _ (Node _ ll xl rl) x r) = avl ll xl (avl rl x r)
rotR _ = undefined

-- Perform a left rotation
rotL :: AVL a -> AVL a
rotL (Node _ l x (Node _ lr xr rr)) = avl (avl l x lr) xr rr
rotL _ = undefined

-- [rebalance t] performs the rotations necessary to re-balance an almost-AVL
-- tree which is skewed by at most 2.
rebalance :: AVL a -> AVL a
rebalance Leaf = Leaf
rebalance m@(Node _ l x d)
    | skew m == 2 && skew l == 1 = rotR m
    | skew m == 2 = rotR $ avl (rotL l) x d
    | skew m == -2 && skew d == -1 = rotL m
    | skew m == -2 = rotL $ avl l x (rotR d)
    | otherwise = m

-- [add t a] inserts an element into an AVL tree, ensuring that the invariants
-- are respected.
add :: (Ord a) => AVL a -> a -> AVL a
add Leaf x = Node 1 Leaf x Leaf
add m@(Node _ l y d) x
    | x < y = rebalance $ avl (add l x) y d
    | x > y = rebalance $ avl l y (add d x)
    | otherwise = m

-- With [add] implemented, we can easily construct an AVL tree from a list
fromList :: (Ord a) => [a] -> AVL a
fromList = foldl add empty

-- [minAVL t] finds the minimum element of [t]
minAVL :: (Ord a) => AVL a -> Maybe a
minAVL Leaf = Nothing
minAVL (Node _ Leaf x _) = Just x
minAVL (Node _ l _ _) = minAVL l

-- [delete t a] returns an AVL tree [t] with [a] removed
delete :: Ord a => AVL a -> a -> AVL a
delete Leaf _ = Leaf
delete (Node _ l x d) y = rebalance $ newTree
    where
        newTree = case compare x y of
            LT -> avl l x (delete d y)
            EQ -> case minAVL d of
                Nothing -> l
                Just min_d -> avl l min_d (delete d min_d)
            GT -> avl (delete l y) x d
