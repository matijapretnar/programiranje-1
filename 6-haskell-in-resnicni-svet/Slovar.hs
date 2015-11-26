module Slovar
    ( Slovar
    , prazen
    , poisci
    , dodaj
    ) where

    import Test.QuickCheck
    import Control.Monad

    data Slovar k v = Slovar [(k, v)] deriving Show

    instance (Arbitrary k, Arbitrary v) => Arbitrary (Slovar k v) where
        arbitrary = liftM Slovar arbitrary

    prazen :: Slovar k v
    prazen = Slovar []

    poisci :: (Eq k) => Slovar k v -> k -> Maybe v
    poisci (Slovar d) k = poisci' d k
      where
        poisci' [] _ = Nothing
        poisci' ((k,v):d) k'
            | k' == k = Just v
            | otherwise = poisci' d k'

    --zlij :: Slovar k v -> Slovar k v -> Slovar k v
    --izSeznama :: [(k, v)] -> Slovar k v
    --dodajSeznam :: Slovar k v -> [(k, v)] -> Slovar k v
    --dodaj :: Slovar k v -> (k, v) -> Slovar k v
    dodaj :: (Eq k) => Slovar k v -> k -> v -> Slovar k v
    dodaj (Slovar d) k v = Slovar (dodaj' d k v)
      where
        -- Domača naloga: napiši boljšo implementacijo
        dodaj' [] k v = [(k, v)]
        dodaj' ((k,v):d) k' v'
            | k' == k = (k', v'):d
            -- Test poisciDodaj2' nam je povedal, da je tu napaka
            -- | otherwise = (k,v') : dodaj' d k' v'
            | otherwise = (k,v) : dodaj' d k' v'
