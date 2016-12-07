import Dict
import Test.QuickCheck


-- If we add a key and value, when we search for the key we get back that
-- value.
prop_addSearch :: (Ord k, Eq v) => k -> v -> Dict k v -> Bool
prop_addSearch k v d = search k (add k v d) == Just v

-- If we add a value for a key k1, then add a value for a different key k2,
-- when we search for the value of k1 we get back the value we added.
prop_addAddSearch_k :: (Ord k, Eq v) => k -> v -> k -> v -> Dict k v -> Property
prop_addAddSearch_k k v k' v' d =
  k /= k'  ==>  (search k $ add k' v' (add k v d)) == Just v

-- If we add a value [v1] for a key [k], then add a value [v2] for the same
-- key, when we search for the value of [k] we get back [v2].
prop_addAddSearch_v :: (Ord k, Eq v) => k -> v -> v -> Dict k v -> Bool
prop_addAddSearch_v k v v' d =
  (search k $ add k v' (add k v d)) == Just v'

prop_removeSearch :: (Ord k, Eq v) => k -> Dict k v -> Bool
prop_removeSearch k d = (search k $ remove k d) == Nothing

tests :: IO ()
tests = do
    quickCheck (prop_addSearch :: Int -> Int -> Dict Int Int -> Bool)
    quickCheck (prop_addSearch :: Int -> String -> Dict Int String -> Bool)
    quickCheck (prop_addAddSearch_k :: Int -> Int -> Int -> Int -> Dict Int Int -> Property)
    quickCheck (prop_addAddSearch_k :: Int -> String -> Int -> String -> Dict Int String -> Property)
    quickCheck (prop_addAddSearch_v :: Int -> Int -> Int -> Dict Int Int -> Bool)
    quickCheck (prop_addAddSearch_v :: Int -> String -> String -> Dict Int String -> Bool)
    quickCheck (prop_removeSearch :: Int -> Dict Int Int -> Bool)
    quickCheck (prop_removeSearch :: Int -> Dict Int String -> Bool)

main :: IO ()
main = do
  tests
