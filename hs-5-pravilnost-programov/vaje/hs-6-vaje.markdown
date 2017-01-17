    import Control.Monad
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
                  liftM3 Sestavljeno subtree arbitrary subtree]
            where subtree = tree' (n `div` 2)

Te definicije ne bom razlagal. Kogar bolj zanima, naj si pogleda dokumentacijo paketa QuickCheck.
    
### Osnovne funkcije na dvojiških drevesih in njihove lastnosti

    -- Sestavite funkcijo prezrcali, ki med seboj zamenja levo in desno
    -- poddrevo (v vseh vozliščih).

    prezrcali :: Drevo a -> Drevo a
    prezrcali = undefined

    -- Sestavite funkcijo globina, ki vrne globino drevesa.

    globina :: Drevo a -> Int
    globina = undefined

    -- Sestavite funkcijo vsota, ki vrne vsoto vseh elementov v drevesu.

    vsota :: Num a => Drevo a -> a
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

    prop_vsotaPrezrcali :: (Eq a, Num a) => Drevo a -> Bool
    prop_vsotaPrezrcali d = undefined

    testi1 = do
        quickCheck (prop_prezrcaliPrezrcali :: Drevo Int -> Bool)
        quickCheck (prop_prezrcaliPrezrcali :: Drevo Char -> Bool)
        quickCheck (prop_globinaPrezrcali :: Drevo Int -> Bool)
        quickCheck (prop_globinaPrezrcali :: Drevo Char -> Bool)
        quickCheck (prop_vsotaPrezrcali :: Drevo Int -> Bool)

Nobena od teh nalog vam ni povzročala težav, tako da posebnih komentarjev nimam.


### Osnovne funkcije na iskalnih drevesih

    -- Slovarje predstavimo z asociativnimi seznami.

    type Slovar k v = [(k, v)]

    -- Definirajte prazen slovar.

    prazen :: Slovar k v
    prazen = undefined

    -- Definirajte metodo poisci, ki v slovarju poišče vrednost danega ključa.

    poisci :: (Eq k) => Slovar k v -> k -> Maybe v
    poisci = undefined

    -- Definirajte metodo dodaj, ki v slovar doda podan ključ in vrednost.

    dodaj :: Slovar k v -> k -> v -> Slovar k v
    dodaj = undefined

Glede na to, da gre tukaj samo za manjšo različico kode s predavanj, večjih težav ni bilo.

