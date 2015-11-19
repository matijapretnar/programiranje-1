import System.Environment

data Mnozica a = P
               | S (Mnozica a) a (Mnozica a)

prazna :: Mnozica a
prazna = P

vsebuje :: (Ord a) => Mnozica a -> a -> Bool
vsebuje P _ = False
vsebuje (S l y d) x
    | x < y = vsebuje l x
    | x > y = vsebuje d x
    | otherwise = True

dodaj :: (Ord a) => Mnozica a -> a -> Mnozica a
dodaj P x = S P x P
dodaj m@(S l y d) x
    | x < y = uravnotezi $ S (dodaj l x) y d
    | x > y = uravnotezi $ S l y (dodaj d x)
    | otherwise = m

uravnotezi :: (Ord a) => Mnozica a -> Mnozica a
uravnotezi P = P
uravnotezi m@(S l x d)
    | razlika m == 2 && razlika l == 1 = rotD m
    | razlika m == 2 = rotD $ S (rotL l) x d
    | razlika m == -2 && razlika d == -1 = rotL m
    | razlika m == -2 = rotL $ S l x (rotD d)
    | otherwise = m

visina :: Mnozica a -> Int
visina P = 0
visina (S l _ d) = 1 + max (visina l) (visina d)

razlika :: Mnozica a -> Int
razlika P = 0
razlika (S l _ d) = visina l - visina d

rotD :: Mnozica a -> Mnozica a
rotD (S (S ll xl dl) x d) = S ll xl (S dl x d)

rotL :: Mnozica a -> Mnozica a
rotL (S l x (S ld xd dd)) = S (S l x ld) xd dd


main =
    do
        arg1:arg2:_ <- getArgs
        let n = read arg1
        let elementi = if arg2 == "n" then map sin [1..n] else [1..n]
        let mnozica = foldl dodaj prazna elementi
        print $ prestej (vsebuje mnozica) elementi
    where
        prestej _ [] = 0
        prestej p (x:xs)
            | p x = 1 + prestej p xs
            | otherwise = prestej p xs
