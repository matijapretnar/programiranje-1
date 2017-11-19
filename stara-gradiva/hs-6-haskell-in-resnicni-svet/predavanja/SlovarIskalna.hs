module Slovar
    ( Slovar
    , prazen
    , dodaj
    , poisci
    ) where
    data Slovar k v = Prazno
                    | Sestavljeno (Slovar k v) (k, v) (Slovar k v)

    prazen :: Slovar k v
    prazen = Prazno

    dodaj :: Eq k => Slovar k v -> k -> v -> Slovar k v
    dodaj Prazno k v = Sestavljeno Prazno (k, v) Prazno
    dodaj (Sestavljeno l (k, v) d) k' v'
        | k' < k = Sestavljeno (dodaj l k' v') (k, v) d
        | k == k' = Sestavljeno l (k, v') d
        | k < k' = Sestavljeno l (k, v) (dodaj d k' v')
