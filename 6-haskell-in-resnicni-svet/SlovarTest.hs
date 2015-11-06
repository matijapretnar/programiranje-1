import Slovar
import Test.QuickCheck

dodajCrke :: Slovar Char Int -> String -> Slovar Char Int
dodajCrke d "" = d
dodajCrke d (crka:niz) = dodajCrke (dodajCrka d crka) niz
  where
    dodajCrka d crka =
        case poisci d crka of
            Nothing -> dodaj d crka 1
            Just n -> dodaj d crka (1 + n)

prop_poisciDodaj :: Slovar Char Int -> Char -> Int -> Bool
prop_poisciDodaj d k v = poisci (dodaj d k v) k == Just v

prop_poisciDodaj2 :: Slovar Char Int -> Char -> Char -> Int -> Int -> Bool
prop_poisciDodaj2 d k k' v v' = poisci (dodaj (dodaj d k v) k' v') k == Just v

--Slovar [('0',-1)]
--'a'
--0

main = do
    quickCheck prop_poisciDodaj2
    --print (dodajCrke prazen "abrakadabra")
