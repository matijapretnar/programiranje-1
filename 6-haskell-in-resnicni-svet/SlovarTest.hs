import Test.QuickCheck
import Slovar

--prop_poisciPrazen :: (Eq k) => k -> Bool
--prop_poisciPrazen k =
--    poisci prazen k == Nothing

--prop_poisciDodajPrazen :: (Ord k, Eq v) => k -> v -> Bool
--prop_poisciDodajPrazen k v =
--    poisci (dodaj prazen k v) k == Just v

prop_poisciDodaj :: (Ord k, Eq v) => Slovar k v -> k -> v -> Bool
prop_poisciDodaj d k v =
    poisci (dodaj d k v) k == Just v

main = do
    --quickCheck (prop_poisciPrazen :: Char -> Bool)
    --quickCheck (prop_poisciPrazen :: Int -> Bool)
    quickCheck (prop_poisciDodaj :: Slovar Int Char -> Int -> Char -> Bool)
    quickCheck (prop_poisciDodaj :: Slovar Char Int -> Char -> Int -> Bool)
