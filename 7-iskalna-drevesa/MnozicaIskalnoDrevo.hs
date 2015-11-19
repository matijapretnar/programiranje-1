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
    | x < y = S (dodaj l x) y d
    | x > y = S l y (dodaj d x)
    | otherwise = m

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
