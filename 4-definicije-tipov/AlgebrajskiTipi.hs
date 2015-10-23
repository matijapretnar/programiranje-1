-- data Bool = False | True

data Naravna = Nic | Naslednik Naravna deriving Show

data Seznam a = Prazen | Sestavljen a (Seznam a) deriving Show

-- data Maybe a = Nothing | Just a

deli :: Double -> Double -> Maybe Double
deli _ 0 = Nothing
deli x y = Just (x / y)

glava :: [a] -> Maybe a
glava [] = Nothing
glava (x:_) = Just x

rep :: [a] -> Maybe [a]
rep [] = Nothing
rep (_:xs) = Just xs


-- data Either a b = Left a | Right b

glava' :: [a] -> Either a String
glava' [] = Right "Ojoj, vzel si glavo praznega seznama."
glava' (x:_) = Left x

type Eksponent a b = b -> a  -- A^B

type Vsota a b = Iota1 a | Iota2 b

-- A^(B + C) = A^B x A^C

phi :: (Vsota b c -> a) -> (b -> a, c -> a)
psi :: (b -> a, c -> a) -> (Vsota b c -> a)


data Drevo a = Prazno
             | Sestavljeno a (Drevo a) (Drevo a)

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno x levi desni) = x + vsota levi + vsota desni
