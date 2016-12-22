import System.Environment

data Mnozica a = M [a]

prazna :: Mnozica a
prazna = M []

vsebuje :: (Eq a) => Mnozica a -> a -> Bool
vsebuje (M m) x = x `elem` m

dodaj :: (Eq a) => Mnozica a -> a -> Mnozica a
dodaj (M m) x = M (x:m)

preveri :: Int -> Int
preveri n =
  length (filter (vsebuje mnozica) elementi)
  where
    elementi = if n < 0 then map pomesaj [1..(-n)] else [1..n]
    mnozica = foldl dodaj prazna elementi
    pomesaj i = 1234567 * i `mod` n
