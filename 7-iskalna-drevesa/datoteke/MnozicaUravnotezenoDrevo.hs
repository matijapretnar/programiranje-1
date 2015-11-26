import System.Environment

data Mnozica a = P
               | S Int (Mnozica a) a (Mnozica a)

prazna :: Mnozica a
prazna = P

-- konstruktor sestavljenih dreves preden smo drevesom dodali višino
-- S :: Mnozica a -> a -> Mnozica a -> Mnozica a

-- konstruktor sstavljenih dreves, ko drevesom dodamo višino
-- S :: Int -> Mnozica a -> a -> Mnozica a -> Mnozica a

-- pametni konstruktor, ki višino izračuna sam in ima isti tip, kot prej S
pametniS :: Mnozica a -> a -> Mnozica a -> Mnozica a
pametniS l x d = S h l x d where h = 1 + max (visina l) (visina d)

vsebuje :: (Ord a) => Mnozica a -> a -> Bool
vsebuje P _ = False
vsebuje (S _ l y d) x
    | x < y = vsebuje l x
    | x > y = vsebuje d x
    | otherwise = True

-- Bedni način z bedno bednimi neumnimi konstruktorji
-- dodaj :: (Ord a) => Mnozica a -> a -> Mnozica a
-- dodaj P x = S 1 P x P
-- dodaj m@(S h l y d) x
--     | x < y =
--         uravnotezi $ S h' l' y d
--         where
--             h' = 1 + max (visina l') (visina d)
--             l' = dodaj l x
--     | x > y = uravnotezi $ S l y (dodaj d x)
--     | otherwise = m

-- Pameten način s pametnimi konstruktorji
dodaj :: (Ord a) => Mnozica a -> a -> Mnozica a
dodaj P x = pametniS P x P
dodaj m@(S _ l y d) x
    | x < y = uravnotezi $ pametniS (dodaj l x) y d
    | x > y = uravnotezi $ pametniS l y (dodaj d x)
    | otherwise = m

uravnotezi :: (Ord a) => Mnozica a -> Mnozica a
uravnotezi P = P
uravnotezi m@(S _ l x d)
    | razlika m == 2 && razlika l == 1 = rotD m
    | razlika m == 2 = rotD $ pametniS (rotL l) x d
    | razlika m == -2 && razlika d == -1 = rotL m
    | razlika m == -2 = rotL $ pametniS l x (rotD d)
    | otherwise = m

visina :: Mnozica a -> Int
visina P = 0
visina (S h _ _ _) = h

razlika :: Mnozica a -> Int
razlika P = 0
razlika (S _ l _ d) = visina l - visina d

rotD :: Mnozica a -> Mnozica a
rotD (S _ (S _ ll xl dl) x d) = pametniS ll xl (pametniS dl x d)

rotL :: Mnozica a -> Mnozica a
rotL (S _ l x (S _ ld xd dd)) = pametniS (pametniS l x ld) xd dd


main =
    do
        args <- getArgs
        let n = if null args then 1000 else read $ head args
        let elementi = if n < 0 then map sin [1..(-n)] else [1..n]
        let mnozica = foldl dodaj prazna elementi
        print $ prestej (vsebuje mnozica) elementi
    where
        prestej _ [] = 0
        prestej p (x:xs)
            | p x = 1 + prestej p xs
            | otherwise = prestej p xs
