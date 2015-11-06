module Slovar
    ( Slovar
    , prazen
    , poisci
    , dodaj
    , testi1
    , testi2
    ) where

import Test.QuickCheck
import Control.Monad
import System.Random

data Slovar a b = Prazno
               | Sestavljeno (Slovar a b) a b (Slovar a b)
               deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Slovar a b) where
  arbitrary = sized tree'
    where
      tree' 0 = return Prazno
      tree' n | n > 0 = 
        oneof [return Prazno,
              liftM4 Sestavljeno subtree arbitrary arbitrary subtree]
        where subtree = tree' (n `div` 2)

--

prazen :: Slovar a b
prazen = Prazno

prezrcali :: Slovar a b -> Slovar a b
prezrcali Prazno = Prazno
prezrcali (Sestavljeno l k v d) = Sestavljeno (prezrcali d) k v (prezrcali l)

globina :: Slovar a b -> Int
globina Prazno = 0
globina (Sestavljeno l _ _ d) = 1 + max (globina l) (globina d)

prop_prezrcaliPrezrcali :: (Eq a, Eq b) => Slovar a b -> Bool
prop_prezrcaliPrezrcali d = prezrcali (prezrcali d) == d

prop_globinaPrezrcali :: (Eq a, Eq b) => Slovar a b -> Bool
prop_globinaPrezrcali d = globina (prezrcali d) == globina d

testi1 = do
    quickCheck (prop_prezrcaliPrezrcali :: Slovar Int Int -> Bool)
    quickCheck (prop_prezrcaliPrezrcali :: Slovar Int Char -> Bool)
    quickCheck (prop_globinaPrezrcali :: Slovar Int Int -> Bool)
    quickCheck (prop_globinaPrezrcali :: Slovar Int Char -> Bool)

--

jeIskalno :: (Ord a) => Slovar a b -> Bool
jeIskalno d = jeIskalno' d Nothing Nothing
  where
    jeIskalno' Prazno _ _ = True
    jeIskalno' (Sestavljeno l x _ d) mini maksi =
        jeIskalno' l mini (Just x)
        && jeVmes mini x maksi
        && jeIskalno' d (Just x) maksi

    jeVmes Nothing _ Nothing = True
    jeVmes Nothing y (Just z) = y < z
    jeVmes (Just x) y Nothing = x < y
    jeVmes (Just x) y (Just z) = x < y && y < z

poisci :: (Ord a) => Slovar a b -> a -> Maybe b
poisci Prazno _ = Nothing
poisci (Sestavljeno l k v d) k'
    | k < k' = poisci d k'
    | k == k' = Just v
    | otherwise = poisci l k'

dodaj :: (Ord a) => Slovar a b -> a -> b -> Slovar a b
dodaj Prazno k v = Sestavljeno Prazno k v Prazno
dodaj (Sestavljeno l k v d) k' v'
    | k < k' = Sestavljeno l k v (dodaj d k' v')
    | k == k' = Sestavljeno l k' v' d
    | otherwise = Sestavljeno (dodaj l k' v') k v d

--

prop_jeIskalnoDodaj :: Slovar Int String -> Int -> String -> Property
prop_jeIskalnoDodaj d k v =
    collect (globina d) $ collect (jeIskalno d) $ sledi (jeIskalno d) (jeIskalno (dodaj d k v))
    where
        sledi True False = False
        sledi _ _ = True

prop_jeIskalnoDodaj' :: Slovar Int String -> Int -> String -> Property
prop_jeIskalnoDodaj' d k v =
    collect (globina d) $ collect (jeIskalno d) $ jeIskalno d ==> jeIskalno (dodaj d k v)

iskalnaDrevesa :: Gen (Slovar Int String)
iskalnaDrevesa =
    sized $ \n -> do
        mini <- arbitrary
        maksi <- arbitrary
        tree' mini maksi n
    where
      tree' mini maksi n
        | n > 0 && mini <= maksi = 
            oneof [return Prazno, do
                    k <- choose (mini, maksi)
                    v <- arbitrary
                    l <- tree' mini (pred k) (n `div` 2)
                    d <- tree' (succ k) maksi (n `div` 2)
                    return (Sestavljeno l k v d)]
        | otherwise = return Prazno

prop_jeIskalnoDodaj'' :: Int -> String -> Property
prop_jeIskalnoDodaj'' k v =
    forAll iskalnaDrevesa $ \d ->
    collect (globina d) $ collect (jeIskalno d) $ jeIskalno (dodaj d k v)

--

prop_globinaDodaj :: Int -> String -> Property
prop_globinaDodaj k v =
    forAll iskalnaDrevesa $ \d -> globina d <= globina (dodaj d k v)

prop_globinaDodaj' :: Int -> String -> Property
prop_globinaDodaj' k v =
    forAll iskalnaDrevesa $ \d -> globina (dodaj d k v) <= globina d + 1

prop_poisciDodaj :: Int -> String -> Property
prop_poisciDodaj k v =
    forAll iskalnaDrevesa $ \d -> poisci (dodaj d k v) k == Just v

prop_poisciDodaj2 :: Int -> String -> Int -> String -> Property
prop_poisciDodaj2 k v k' v' =
    forAll iskalnaDrevesa $ \d -> k /= k' ==> poisci (dodaj (dodaj d k v) k' v') k == Just v

testi2 = do
    quickCheck prop_jeIskalnoDodaj
    quickCheck prop_jeIskalnoDodaj'
    quickCheck prop_jeIskalnoDodaj''
    quickCheck prop_globinaDodaj
    quickCheck prop_globinaDodaj'
    quickCheck prop_poisciDodaj
    quickCheck prop_poisciDodaj2
