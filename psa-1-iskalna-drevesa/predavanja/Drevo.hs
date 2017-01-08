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
dodaj (S l y d) x
    | x < y = S (dodaj l x) y d
    | x > y = S l y (dodaj d x)
    | otherwise = S l y d

preveri :: Int -> Int
preveri n =
  length (filter (vsebuje mnozica) elementi)
  where
    elementi = if n < 0 then map pomesaj [1..(-n)] else [1..n]
    mnozica = foldl dodaj prazna elementi
    pomesaj i = 1234567 * i `mod` n
