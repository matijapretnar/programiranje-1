module Slovar
    ( Slovar
    , prazen
    , dodaj
    , poisci
    ) where
    data Slovar k v = Slovar [(k, v)]

    prazen :: Slovar k v
    prazen = Slovar []

    dodaj :: Eq k => Slovar k v -> k -> v -> Slovar k v
    dodaj (Slovar []) k v = (Slovar [(k, v)])
    dodaj (Slovar ((k, v):s)) k' v'
        | k == k' = Slovar ((k, v') : s)
        | otherwise =
            case dodaj (Slovar s) k' v' of
                Slovar s -> Slovar ((k, v) : s)

    poisci :: Eq k => Slovar k v -> k -> Maybe v
    poisci (Slovar []) _ = Nothing
    poisci (Slovar ((k, v):s)) k'
        | k == k' = Just v
        | otherwise = poisci (Slovar s) k'

    pobrisi :: Slovar k v -> k -> Slovar k v
    pobrisi = undefined
