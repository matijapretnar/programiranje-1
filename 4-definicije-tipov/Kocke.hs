--type String = [Char]

data Barva = Rdeca
           | Rumena
           | Modra
           | Bela
           | Crna
           | Siva
           | Druga String
           deriving (Eq, Show, Ord)

imeBarve :: Barva -> String
imeBarve Rdeca = "rdeca"
imeBarve Rumena = "rumena"
imeBarve Modra = "modra"
imeBarve Bela = "bela"
imeBarve Crna = "crna"
imeBarve Siva = "siva"
imeBarve (Druga ime) = ime

zmesaj :: Barva -> Barva -> Barva
zmesaj Bela Crna = Siva
zmesaj Crna Bela = Siva
zmesaj barva1 barva2
    | barva1 == barva2 = barva1
    | barva1 > barva2 = zmesaj barva2 barva1
    | otherwise = Druga ((init $ imeBarve barva1) ++ "o-" ++ imeBarve barva2)



data Kocka = Ploscica Int Int Barva
           | Kockica Int Int Barva
           | Figurica String [Barva]
           | Posebna String Barva
           | Zlepljena [Kocka]
           deriving Show

type Kocke = [Kocka]

mojeKocke = [Kockica 2 4 Rdeca,
             Ploscica 2 4 Rdeca,
             Ploscica 2 4 (Druga "zelena"),
             Posebna "volanček" (Druga "oranžna"),
             Figurica "gasilec" [Crna, Rdeca]]

zanimiva (Ploscica _ _ (Druga _)) = True
zanimiva (Kockica _ _ (Druga _)) = True
zanimiva (Figurica _ _) = True
zanimiva (Posebna _ _) = True
zanimiva _ = False
