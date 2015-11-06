module Slovar
  ( Slovar
  , dodaj
  , poisci
  , prazen
  ) where

    data Slovar k v = Slovar [(k, v)] deriving Show

    dodaj :: Slovar k v -> k -> v -> Slovar k v
    dodaj (Slovar d) k v = Slovar ((k, v) : d)

    poisci :: Eq k => Slovar k v -> k -> Maybe v
    poisci (Slovar []) _ = Nothing
    poisci (Slovar ((k,v):d)) k'
        | k == k' = Just v
        | otherwise = poisci (Slovar d) k'

    prazen :: Slovar k v
    prazen = Slovar []



module Slovar
  ( Slovar
  , dodaj
  , poisci
  , prazen
  , moj
  ) where

    import Test.QuickCheck
    import Control.Monad

    instance  (Arbitrary k, Arbitrary v) => Arbitrary (Slovar k v)  where
       arbitrary = liftM Slovar arbitrary

    data Slovar k v = Slovar [(k, v)] deriving Show

    dodaj :: Ord k => Slovar k v -> k -> v -> Slovar k v
    dodaj (Slovar d) k v = Slovar (dodaj' d k v)
      where
        dodaj' [] k v = [(k, v)]
        dodaj' ((k, v):d) k' v'
            | k < k' = (k, v) : dodaj' d k' v'
            | k == k' = (k', v') : d
            | otherwise = (k', v') : (k, v) : d

    poisci :: Ord k => Slovar k v -> k -> Maybe v
    poisci (Slovar d) k = poisci' d k
      where
        poisci' [] _ = Nothing
        poisci' ((k,v):d) k'
            | k < k' = poisci' d k'
            | k == k' = Just v
            | otherwise = Nothing

    prazen :: Slovar k v
    prazen = Slovar []

    moj = Slovar [('Z',-1),('\136',-5),('|',-1),('^',-3)]
