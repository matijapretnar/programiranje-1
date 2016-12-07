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
flipTree = undefined

-- Compute the depth of a tree
depth :: Tree a -> Int
depth = undefined

-- Calculate the sum of the elements of a tree
sumTree :: Num a => Tree a -> a
sumTree = undefined

-- If you flip a tree twice, you get back the same tree. Write a predicate
-- asserting this property.
prop_flip2id :: Eq a => Tree a -> Bool
prop_flip2id = undefined

-- Write a predicate that checks that if you flip a tree, its depth does not
-- change.
prop_flipDepth :: Tree a -> Bool
prop_flipDepth = undefined

-- Write a predicate to check that flipping a tree does not change its sum.
prop_flipSum :: (Eq n, Num n) => Tree n -> Bool
prop_flipSum = undefined

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
empty = undefined

-- Define a search function, that find the value associated to a key if it is
-- present in a dictionary.
search :: Eq a => a -> [(a, b)] -> Maybe b
search = undefined

-- Define an add function that adds a key value pair to a dictionary.
add :: k -> v -> Dict k v -> Dict k v
add = undefined

-- If we add a key and value, when we search for the key we get back that
-- value.
prop_addSearch :: (Eq k, Eq v) => k -> v -> Dict k v -> Bool
prop_addSearch = undefined

-- If we add a value for a key k1, then add a value for a different key k2,
-- when we search for the value of k1 we get back the value we added.
prop_addAddSearch :: (Eq k, Eq v) => k -> v -> k -> v -> Dict k v -> Property
prop_addAddSearch = undefined

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
