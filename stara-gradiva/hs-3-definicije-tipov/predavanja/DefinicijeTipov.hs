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
           deriving Show

type Kocke = [Kocka]

mojeKocke :: Kocke
mojeKocke = [Kockica 2 4 Rdeca,
             Ploscica 2 4 Rdeca,
             Ploscica 2 4 (Druga "zelena"),
             Posebna "volanček" (Druga "oranžna"),
             Figurica "gasilec" [Crna, Rdeca]]

zanimiva :: Kocka -> Bool
zanimiva (Ploscica _ _ (Druga _)) = True
zanimiva (Kockica _ _ (Druga _)) = True
zanimiva (Figurica _ _) = True
zanimiva (Posebna _ _) = True
zanimiva _ = False


-- data Bool = False
--           | True

data Naravna = Nic
             | Naslednik Naravna
             deriving Show

data Seznam a = Prazen
              | Sestavljen a (Seznam a)
              deriving Show

-- data Maybe a = Nothing
--              | Just a

deli :: Double -> Double -> Maybe Double
deli _ 0 = Nothing
deli x y = Just (x / y)

glava :: [a] -> Maybe a
glava [] = Nothing
glava (x:_) = Just x

rep :: [a] -> Maybe [a]
rep [] = Nothing
rep (_:xs) = Just xs


-- data Either a b = Left a
--                 | Right b

-- Either običajno uporabljamo namesto Maybe, kadar v primeru napake
-- želimo vrniti tudi opis, ne zgolj Nothing.

glava' :: [a] -> Either String a
glava' [] = Left "Ojoj, vzel si glavo praznega seznama."
glava' (x:_) = Right x

-- Konstruktor tipov Either nam predstavlja disjunktno unijo tipov.
-- Spomnimo se, da sta množici C^(A + B) in C^A x C^B izomorfni.
-- Ta izomorfizem lahko prikažemo tudi v Haskellu:

phi :: (Either a b -> c) -> (a -> c, b -> c)
phi f = (\x -> f (Left x), \y -> f (Right y))

psi :: (a -> c, b -> c) -> (Either a b -> c)
psi (g1, g2) = h where
    h (Left x) = g1 x
    h (Right y) = g2 y


data Drevo a = Prazno
             | Sestavljeno a (Drevo a) (Drevo a)

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno x levi desni) = x + vsota levi + vsota desni
