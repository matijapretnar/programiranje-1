{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- V tem modulu sestavimo slovar s pomočjo iskalnih dreves.

module Slovar
  ( Slovar
  , prazen
  , dodaj
  , poisci
  , odstrani
  , izSeznama
  )
  where

import Test.QuickCheck
import Control.Monad

data Drevo a = Prazno | Sestavljeno (Drevo a) a (Drevo a)  deriving (Eq, Show)

type Slovar k v = Drevo (k, v)

-- Implementiraj prazen seznam.
prazen :: Slovar k v
prazen = undefined

-- [dodaj k v d] v slovar [d] doda par ključ: [k], vrednost: [v]
dodaj :: Ord k => k -> v -> Slovar k v -> Slovar k v
dodaj k v d = undefined

-- [poisci k d] v slovarju [d] poisce vrednost ključa k, če ta sploh obstaja
poisci :: Ord k => k -> Slovar k v -> Maybe v
poisci k d = undefined

-- [vstaviPoddrevoDesno pd d] vstavi drevo [pd] na najbolj desno mesto v drevesu [d]
vstaviPoddrevoDesno :: Drevo a -> Drevo a -> Drevo a
vstaviPoddrevoDesno pd d = undefined

-- [odstrani k d] vrne slovar, v katerem ključ [k] nima določene vrednosti
odstrani :: Ord k => k -> Slovar k v -> Slovar k v
odstrani k d = undefined

-- Sestavi funkcijo [izSeznama], ki iz seznama parov [(k,v)] sestavi slovar. 
-- Predpostavi, da so v danem seznamu vsi ključi različni.
-- Nasvet: foldl
izSeznama :: Ord k => [(k,v)] -> Slovar k v
izSeznama = undefined

-- Naključna drevesa v QuickChecku lahko generiramo s spodnjo funkcijo...
arbitraryTree :: (Arbitrary a) => Gen (Drevo a)
arbitraryTree = sized genTree
    where genTree n = case compare n 0 of
            LT -> return Prazno
            EQ -> return Prazno
            GT -> oneof [return Prazno,
                         liftM3 Sestavljeno subtree arbitrary subtree]
              where subtree = genTree (n `div` 2)

-- ...vendar je ne bomo uporabili, ker ne generira iskalnih dreves.
-- instance Arbitrary a => Arbitrary (Drevo a) where
--   arbitrary = arbitraryTree

-- Namesto tega bomo uporabljali spodnji generator.

-- Pravilnost vseh testov na iskalnih drevesih se zanaša na pravilnost generatorja
-- iskalnih dreves. V našem primeru ze zanašamo na funkcijo [izSeznama] ter
-- funkcijo [dodaj]. Če jima ne zaupamo, lahko napišemo bolj zapleten generator,
-- ki ju ne potrebuje.
genSearchTree :: (Ord k, Arbitrary k, Arbitrary v) => Gen (Slovar k v)
genSearchTree = liftM izSeznama arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Slovar k v) where
    arbitrary = genSearchTree

jeUrejen :: (Ord k) => Slovar k v -> Bool
jeUrejen d = jeUrejenMed Nothing d Nothing where
  jeUrejenMed _ Prazno _ = True
  jeUrejenMed minK (Sestavljeno l (k, _) r) maxK =
    minK <=? Just k && Just k <=? maxK &&
    jeUrejenMed minK l (Just k) &&
    jeUrejenMed (Just k) r maxK
  Nothing <=? _ = True
  _ <=? Nothing = True
  Just x <=? Just y = x <= y

-- Če [dodaj] deluje pravilno, bi morali ti testi biti uspešni.
prop_jeUrejenDodaj :: Ord k => Slovar k v -> k -> v -> Property
prop_jeUrejenDodaj d k v =
    (jeUrejen d) ==> (jeUrejen (dodaj k v d))

internalTests :: IO ()
internalTests = do
  quickCheck (jeUrejen :: Slovar Integer String -> Bool)
  quickCheck (jeUrejen :: Slovar String Integer -> Bool)
  quickCheck (prop_jeUrejenDodaj :: Slovar Integer String -> Integer -> String -> Property)
  quickCheck (prop_jeUrejenDodaj :: Slovar String Integer -> String -> Integer -> Property)
