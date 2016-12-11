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

-- Nazadnje je tule še generator naključnih iskalnih dreves, ki ga potrebuje QuickCheck:
instance (Arbitrary a) => Arbitrary (Drevo a) where
  arbitrary = sized tree'
    where tree' n = case compare n 0 of
            LT -> return Prazno
            EQ -> return Prazno
            GT -> oneof [return Prazno,
                         liftM3 Sestavljeno subtree arbitrary subtree]
              where subtree = tree' (n `div` 2)