{-
 - Vaja 3: Podatkovni tipi
 -}

-- Drevesa
-- =======

-- Spodaj je definiran rekurzivni podatkovni tip Drevo. Dodali bomo še nekaj
-- funkcij za delo s tipom Drevo. Kot zgled je že definirana funkcija vsota, ki
-- izračuna vsoto vseh elementov v drevesu.

data Drevo a = Prazno | Sestavljeno a (Drevo a) (Drevo a)

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno x levo desno) = x + vsota levo + vsota desno

-- Da se drevesa lahko izpišejo na zaslon, uporabimo naslednjo kodo.

instance (Show a) => Show (Drevo a) where
    show Prazno = "Prazno"
    show (Sestavljeno x Prazno Prazno) = "Sestavljeno " ++ show x ++ " Prazno Prazno"
    show (Sestavljeno x Prazno desno) = "Sestavljeno " ++ show x ++ " Prazno (" ++ show desno ++ ")"
    show (Sestavljeno x levo Prazno) = "Sestavljeno " ++ show x ++ " (" ++ show levo ++ ") Prazno"
    show (Sestavljeno x levo desno) = "Sestavljeno " ++ show x ++ " (" ++ show levo ++ ") (" ++ show desno ++ ")"

-- Sestavite funkcijo globina, ki vrne globino drevesa. Prazno drevo ima globino 0.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> globina d
-- 3

globina = undefined

-- Sestavite funkcijo steviloElementov, ki vrne število elementov v drevesu.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> steviloElementov d
-- 4

steviloElementov = undefined

-- Sestavite funkcijo prezrcali, ki drevo prezrcali, tako da pri vsakem vozlišču zamenja levo in desno poddrevo.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> prezrcali d
-- Sestavljeno 3 (Sestavljeno 8 Prazno Prazno) (Sestavljeno 7 (Sestavljeno 2 Prazno Prazno) Prazno)

prezrcali = undefined

-- Sestavite funkcijo, ki drevo prezrcali, tako da levo postane desno, desno pa levo.
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> prezrcali d
-- Sestavljeno 3 (Sestavljeno 8 Prazno Prazno) (Sestavljeno 7 (Sestavljeno 2 Prazno Prazno) Prazno)

najboljLevi = undefined



-- Kompleksna števila
-- ==================

-- Definiran je podatkovni tip Kompleksno, ki predstavlja kompleksno število.
-- Dodali bomo še nekaj funkcij za delo s kompleksnimi števili.

data Kompleksno = Kompleksno Double Double

-- Sestavite funkcijo, ki vrne realni del kompleksnega števila.

re = undefined

-- Sestavite funkcijo, ki vrne imaginarni del kompleksnega števila.

im = undefined

-- Sestavite funkcijo, ki izračuna konjugirano kompleksno število.

konjugiraj = undefined

-- Napravite podatkovni tip Kompleksno kot primerek razreda števil Num.

instance Num Kompleksno where
    (Kompleksno x1 y1) + (Kompleksno x2 y2) = undefined

-- Poskrbite, da se kompleksna števila izpišejo na zaslon (v obliki 3 + 5i).

instance Show Kompleksno where



-- Polinomi
-- ========

-- Definiran je podatkovni tip Polinom, ki predstavlja polinom nad kolobarjem
-- celih števil. Dodali bomo še nekaj funkcij za delo s polinomi.

data Polinom = Polinom [Integer]

x :: Polinom
x = Polinom [0, 1]

-- Sestavite funkcijo, ki izračuna polinom v dani točki. 
--
-- Zgled:
-- ghci> let p = Polinom [2,0,-1]
-- ghci> eval p 2
-- -2

eval = undefined

-- Sestavite funkcijo, ki izračuna odvod polinoma (v točki x).

odvod = undefined

-- Sestavite funkcijo, ki izračuna nedoločeni integral polinoma.

integral = undefined

-- Napravite podatkovni tip Polinom kot primerek razreda Num.

instance Num Polinom where
    signum = error "Polinom: operacija signum nima smisla"

-- Poskrbite, da se polinomi izpišejo na zaslon.
    
instance Show Polinom where
