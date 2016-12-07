module Dict
  ( Dict
  , empty
  , add
  , search
  , remove
  , fromList
  )
  where

import Test.QuickCheck
import Control.Monad

data Tree a = Leaf | Node (Tree a) a (Tree a)  deriving (Eq, Show)

type Dict k v = Tree (k, v)

-- An empty dictionary.
empty :: Dict k v
empty = Leaf

-- [add k v d] associates [v] to [k] in a dictionary [d].
add :: Ord k => k -> v -> Dict k v -> Dict k v
add k v Leaf = Node Leaf (k,v) Leaf
add k v (Node l a@(k',_) r) =
  case compare k k' of
    LT -> Node (add k v l) a r
    EQ -> Node l (k,v) r
    GT -> Node l a (add k v r)

-- [search k d] finds the value associated to a key [k], if it is present in a
-- dictionary.
search :: Ord k => k -> Dict k v -> Maybe v
search _ Leaf = Nothing
search k (Node l (k', v') r) = case compare k k' of
  LT -> search k l
  GT -> search k r
  EQ -> Just v'

-- [insert_sub_r sub t] inserts [sub] as a the rightmost subtree in [t].
insertSubR :: Tree a -> Tree a -> Tree a
insertSubR sub Leaf = sub
insertSubR sub (Node l a r)  = Node l a (insertSubR sub r)

-- [remove k d] returns [d] where [k] does not have an associated value.
remove :: Ord k => k -> Dict k v -> Dict k v
remove _ Leaf = Leaf
remove k (Node l a@(k',_) r) =
  case compare k k' of
    LT -> Node (remove k l) a r
    EQ -> insertSubR r l
    GT -> Node l a (remove k r)

-- Produce a dictionary from a list. The keys in the list are assumed to be
-- without repetition.
fromList :: Ord k => [(k,v)] -> Dict k v
fromList = foldl (flip $ uncurry add) empty


-- We provide a generator for random trees for QuickCheck.
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' n = case compare n 0 of
            LT -> return Leaf
            EQ -> return Leaf
            GT -> oneof [return Leaf,
                         liftM3 Node subtree arbitrary subtree]
              where subtree = tree' (n `div` 2)
