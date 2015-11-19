import System.Environment

data Mnozica a = M [a]

prazna :: Mnozica a
prazna = M []

vsebuje :: (Eq a) => Mnozica a -> a -> Bool
vsebuje (M m) x = x `elem` m

dodaj :: (Eq a) => Mnozica a -> a -> Mnozica a
dodaj (M m) x = M (x:m)


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
