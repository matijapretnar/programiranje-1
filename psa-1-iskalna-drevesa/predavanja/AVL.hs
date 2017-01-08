import System.Environment

data Mnozica a = P
               | S Int (Mnozica a) a (Mnozica a)

prazna :: Mnozica a
prazna = P

vsebuje :: (Ord a) => Mnozica a -> a -> Bool
vsebuje P _ = False
vsebuje (S _ l y d) x
    | x < y = vsebuje l x
    | x > y = vsebuje d x
    | otherwise = True

s l x d = S h l x d
  where
    h = 1 + max (visina l) (visina d)

dodaj :: (Ord a) => Mnozica a -> a -> Mnozica a
dodaj P x = s P x P
dodaj m@(S _ l y d) x
    | x < y = uravnotezi $ s (dodaj l x) y d
    | x > y = uravnotezi $ s l y (dodaj d x)
    | otherwise = m

uravnotezi :: (Ord a) => Mnozica a -> Mnozica a
uravnotezi P = P
uravnotezi m@(S _ l x d)
    | razlika m == 2 && razlika l == 1 = rotD m
    | razlika m == 2 = rotD $ s (rotL l) x d
    | razlika m == -2 && razlika d == -1 = rotL m
    | razlika m == -2 = rotL $ s l x (rotD d)
    | otherwise = m

visina :: Mnozica a -> Int
visina P = 0
visina (S h _ _ _) = h

razlika :: Mnozica a -> Int
razlika P = 0
razlika (S _ l _ d) = visina l - visina d

rotD :: Mnozica a -> Mnozica a
rotD (S _ (S _ ll xl dl) x d) = s ll xl (s dl x d)

rotL :: Mnozica a -> Mnozica a
rotL (S _ l x (S _ ld xd dd)) = s (s l x ld) xd dd

preveri :: Int -> Int
preveri n =
  length (filter (vsebuje mnozica) elementi)
  where
    elementi = if n < 0 then map pomesaj [1..(-n)] else [1..n]
    mnozica = foldl dodaj prazna elementi
    pomesaj i = 1234567 * i `mod` n
