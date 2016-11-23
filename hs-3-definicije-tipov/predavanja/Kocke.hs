data Barva = Modra
           | Zelena
           | Rdeca
           | Rumena
           | Druga String
           deriving (Eq, Ord, Show)

zmesaj :: Barva -> Barva -> Barva
zmesaj barva1 barva2
    | barva1 > barva2 = zmesaj barva2 barva1
    | barva1 == barva2 = barva1
zmesaj Modra Rumena = Zelena
zmesaj barva1 barva2 = Druga (ime barva1 ++ "-" ++ ime barva2)

ime :: Barva -> String
ime Zelena = "zelena"
ime Rdeca = "rdeca"
ime Rumena = "rumena"
ime Modra = "modra"
ime (Druga opis) = opis


data Kocka = Kockica Int Int Barva
           | Ploscica Int Int Barva
           | Figurica String
           | Posebna String Barva

jePosebna :: Kocka -> Bool
jePosebna (Kockica _ _ (Druga _)) = True
jePosebna (Kockica sirina dolzina _) = sirina > 100 || dolzina > 100
jePosebna (Ploscica _ _ (Druga _)) = True
jePosebna (Ploscica sirina dolzina _) = sirina > 100 || dolzina > 100
jePosebna (Figurica _) = True
jePosebna (Posebna _ _) = True


varnoDeljenje :: Int -> Int -> Maybe Int
varnoDeljenje _ 0 = Nothing
varnoDeljenje m n = Just (m `div` n)

povprecje :: [Int] -> Maybe Int
povprecje xs = varnoDeljenje (sum xs) (length xs)
