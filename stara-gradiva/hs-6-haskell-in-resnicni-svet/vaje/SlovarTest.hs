-- Tukaj testiramo modul Slovar s QuickCheck.

import Slovar
import Test.QuickCheck


-- Ko v slovar dodamo ključ in vrednost, za tem pa iščemo vrednost pri tem
-- ključu, dobimo nazaj vrednost, ki smo jo predpisali prej.
prop_dodajPoisci :: (Ord k, Eq v) => k -> v -> Slovar k v -> Bool
prop_dodajPoisci k v d = undefined

-- Ko v slovar dodamo vrednosti pri ključu k1 in potem pri nekem drugačnem
-- ključu k2, za tem pa iščemo vrednost pri k1, dobimo nazaj vrednost, ki smo jo
-- predpisali prej.
prop_dodajPoisciPriDrugem :: (Ord k, Eq v) => k -> v -> k -> v -> Slovar k v -> Property
prop_dodajPoisciPriDrugem k v k' v' d = undefined

-- Ko v slovar dodamo vrednost v1 pri ključu k in potem dodamo vrednost v2 pri
-- istem ključu, za tem pa iščemo vrednost pri k, dobimo nazaj vrednost v2.
prop_dodajDodajPoisci :: (Ord k, Eq v) => k -> v -> v -> Slovar k v -> Bool
prop_dodajDodajPoisci k v v' d = undefined

-- Ko iz slovarja odstranimo ključ k, potem pa iščemo vrednost pri tem ključu,
-- dobimo nazaj Nothing.
prod_odstraniPoisci :: (Ord k, Eq v) => k -> Slovar k v -> Bool
prod_odstraniPoisci k d = undefined

testi :: IO ()
testi = do
    quickCheck (prop_dodajPoisci :: Int -> String -> Slovar Int String -> Bool)
    quickCheck (prop_dodajPoisciPriDrugem :: Int -> String -> Int -> String -> Slovar Int String -> Property)
    quickCheck (prop_dodajDodajPoisci :: Int -> String -> String -> Slovar Int String -> Bool)
    quickCheck (prod_odstraniPoisci :: Int -> Slovar Int String -> Bool)

main :: IO ()
main = do
  testi
