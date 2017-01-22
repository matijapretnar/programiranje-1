{-
 - Vaja 3: Podatkovni tipi
 -}


-- Naravna števila
-- ===============

data Naravno = Nic | Nasl Naravno deriving (Show)

-- Sestavite funkcijo sestej, ki vrne vsoto naravnih števil.

sestej :: Naravno -> Naravno -> Naravno
sestej = undefined

-- Sestavite funkcijo zmnoži, ki vrne zmnožek naravnih števil.

zmnozi :: Naravno -> Naravno -> Naravno
zmnozi = undefined

-- Sestavite funkcijo vNaravno, ki Integer pretvori v naravno število
--
-- Zgled:
-- ghci> vNaravno 0
-- Nic
-- ghci> vNaravno 2
-- Nasl (Nasl Nic)

vNaravno :: Integer -> Naravno
vNaravno = undefined

-- Sestavite funkcijo izNaravnega, ki naravno število pretvori v Integer
-- 
-- Zgled:
-- ghci> izNaravnega Nic
-- 0
-- ghci> izNaravnega (Nasl (Nasl Nic))
-- 2

izNaravnega :: Naravno -> Integer
izNaravnega = undefined



-- Drevesa
-- =======

-- Spodaj je definiran rekurzivni podatkovni tip Drevo. Dodali bomo še nekaj
-- funkcij za delo s tipom Drevo. Kot zgled je že definirana funkcija vsota, ki
-- izračuna vsoto vseh elementov v drevesu.

data Drevo a = Prazno | Sestavljeno a (Drevo a) (Drevo a) deriving (Show)

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno x levo desno) = x + vsota levo + vsota desno

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

-- Sestavite funkcijo, ki vrne najblj levi element v drevesu.
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> najboljLevi d
-- 7

najboljLevi = undefined



-- Kompleksna števila
-- ==================

-- Definiran je podatkovni tip Kompleksno, ki predstavlja kompleksno število.
-- Dodali bomo še nekaj funkcij za delo s kompleksnimi števili.

data Kompleksno = Kompleksno Double Double deriving (Show)

-- Sestavite funkcijo, ki vrne realni del kompleksnega števila.

re = undefined

-- Sestavite funkcijo, ki vrne imaginarni del kompleksnega števila.

im = undefined

-- Sestavite funkcijo, ki izračuna konjugirano kompleksno število.

konjugiraj = undefined


-- Polinomi
-- ========

-- Definiran je podatkovni tip Polinom, ki predstavlja polinom nad obsegom
-- racionalnih števil. Dodali bomo še nekaj funkcij za delo s polinomi.

data Polinom = Polinom [Rational] deriving (Show)

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
