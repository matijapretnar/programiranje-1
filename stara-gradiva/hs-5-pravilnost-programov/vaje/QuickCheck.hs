import Control.Monad
import Test.QuickCheck

data Tree a = Leaf | Node (Tree a) a (Tree a)  deriving (Eq, Show)

-- We provide a generator for random trees for QuickCheck
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' 0         = return Leaf
          tree' n | n > 0 =
            oneof [return Leaf,
                    liftM3 Node subtree arbitrary subtree]
            where subtree = tree' (n `div` 2)

-- Reverse all left and right subtrees of a tree
flipTree :: Tree a -> Tree a
flipTree Leaf = Leaf
flipTree (Node l a r) = Node (flipTree r) a (flipTree l)

-- Compute the depth of a tree
depth :: Tree a -> Int
depth Leaf = 0
depth (Node l _ r) = max (depth l) (depth r)

-- Calculate the sum of the elements of a tree
sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node l n r) = n + (sumTree l) + (sumTree r)

-- If you flip a tree twice, you get back the same tree. Write a predicate
-- asserting this property.
prop_flip2id :: Eq a => Tree a -> Bool
prop_flip2id t = t == (flipTree $ flipTree t)

-- Write a predicate that checks that if you flip a tree, its depth does not
-- change.
prop_flipDepth :: Tree a -> Bool
prop_flipDepth t = depth t == depth (flipTree t)

-- Write a predicate to check that flipping a tree does not change its sum.
prop_flipSum :: (Eq n, Num n) => Tree n -> Bool
prop_flipSum t = sumTree t == sumTree (flipTree t)

tests1 :: IO ()
tests1 = do
    quickCheck (prop_flip2id :: Tree Int -> Bool)
    quickCheck (prop_flip2id :: Tree Char -> Bool)
    quickCheck (prop_flipDepth :: Tree Int -> Bool)
    quickCheck (prop_flipDepth :: Tree Char -> Bool)
    quickCheck (prop_flipSum :: Tree Int -> Bool)



-- Dictionaries represented by associative lists.

type Dict k v = [(k, v)]

-- Define an empty dictionary.
empty :: Dict k v
empty = []

-- Define a search function, that find the value associated to a key if it is
-- present in a dictionary.
search :: Eq a => a -> [(a, b)] -> Maybe b
search = lookup

-- Define an add function that adds a key value pair to a dictionary.
add :: (Eq k) => k -> v -> Dict k v -> Dict k v
add k v d = case d of 
        [] -> [(k,v)]
        ((k',v'):ds) -> if k == k' then ((k,v):ds) else ((k',v'):(add k v ds))

-- If we add a key and value, when we search for the key we get back that
-- value.
prop_addSearch :: (Eq k, Eq v) => k -> v -> Dict k v -> Bool
prop_addSearch k v d = search k (add k v d) == Just v

-- If we add a value for a key k1, then add a value for a different key k2,
-- when we search for the value of k1 we get back the value we added.
prop_addAddSearch :: (Eq k, Eq v) => k -> v -> k -> v -> Dict k v -> Property
prop_addAddSearch k v k' v' d =
  k /= k' ==>
  let d_added = add k' v' $ add k v d in
    search k d_added == Just v

tests2 :: IO ()
tests2 = do
    quickCheck (prop_addSearch :: Int -> Int -> Dict Int Int -> Bool)
    quickCheck (prop_addSearch :: Int -> String -> Dict Int String -> Bool)
    quickCheck (prop_addAddSearch :: Int -> Int -> Int -> Int -> Dict Int Int -> Property)
    quickCheck (prop_addAddSearch :: Int -> String -> Int -> String -> Dict Int String -> Property)

main :: IO ()
main = do
  tests1
  tests2
