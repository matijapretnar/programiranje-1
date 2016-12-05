import Test.QuickCheck


data Drevo a = Prazno
             | Sestavljeno (Drevo a) a (Drevo a)
             deriving (Eq, Show)

              
--                
-- Tole poskrbi, da si QuickCheck zna izmisliti naključno drevo.               
-- 
instance (Arbitrary a) => Arbitrary (Drevo a) where
  arbitrary = sized tree'
    where
      tree' 0 = return Prazno
      tree' n | n > 0 = 
        oneof [return Prazno,
              liftM4 Sestavljeno subtree arbitrary subtree]
        where subtree = tree' (n `div` 2)

       
       
-- Sestavite funkcijo prezrcali, ki med seboj zamenja levo in desno
-- poddrevo (v vseh vozliščih).

prezrcali :: Drevo a -> Drevo a
prezrcali = undefined

-- Sestavite funkcijo globina, ki vrne globino drevesa.

globina :: Drevo a -> Int
globina = undefined

-- Sestavite funkcijo vsota, ki vrne vsoto vseh elementov v drevesu.

vsota :: Num a => Drevo a -> Int
vsota = undefined

-- Če drevo dvakrat prezrcalimo, dobimo spet isto nazaj. Napišite ustrezno
-- lastnost prop_prezrcaliPrezrcali.

prop_prezrcaliPrezrcali :: Eq a => Drevo a -> Bool
prop_prezrcaliPrezrcali d = undefined

-- Če drevo prezrcalimo, se mu globina pri tem ne spremeni. Napišite ustrezno
-- lastnost prop_globinaPrezrcali.

prop_globinaPrezrcali :: Drevo a -> Bool
prop_globinaPrezrcali d = undefined

-- Če drevo prezrcalimo, se mu vsota pri tem ne spremeni. Napišite ustrezno
-- lastnost prop_vsotaPrezrcali.

prop_vsotaPrezrcali :: Num a => Drevo a -> Bool
prop_vsotaPrezrcali d = undefined

testi1 = do
    quickCheck (prop_prezrcaliPrezrcali :: Drevo Int -> Bool)
    quickCheck (prop_prezrcaliPrezrcali :: Drevo Char -> Bool)
    quickCheck (prop_globinaPrezrcali :: Drevo Int -> Bool)
    quickCheck (prop_globinaPrezrcali :: Drevo Char -> Bool)
    quickCheck (prop_vsotaPrezrcali :: Drevo Int -> Bool)
    quickCheck (prop_vsotaPrezrcali :: Drevo Char -> Bool)

-- Slovarje predstavimo z asociativnimi seznami.

type Slovar k v = [(k, v)]

-- Definirajte prazen slovar.

prazen :: Slovar k v
prazen = undefined

-- Definirajte metodo poisci, ki v slovarju poišče vrednost danega ključa.

poisci :: (Ord k) => Slovar k v -> k -> Maybe v
poisci = undefined

-- Definirajte metodo dodaj, ki v slovar doda podan ključ in vrednost.

dodaj :: (Ord k) => Slovar k v -> k -> v -> Slovar k v
dodaj = undefined

-- Če smo v slovar ravnokar vstavili nek ključ in vrednost, moramo dobiti
-- pri iskanju tega ključa vrednost, ki smo jo ravnokar vstavili.

prop_poisciDodaj :: Slovar k v -> k -> v -> Property
prop_poisciDodaj s k v = undefined

-- Če vstavimo nek ključ in vrednost v in k ter takoj zatem že nek par
-- v' in k', kjer je v' različna od v, potem moramo pri iskanju vrednosti
-- od ključa k dobiti v.

prop_poisciDodaj2 :: Slovar k v -> k -> v -> k -> v -> Property
prop_poisciDodaj2 s k v k' v' = undefined

testi2 = do
    quickCheck prop_poisciDodaj
    quickCheck prop_poisciDodaj2
