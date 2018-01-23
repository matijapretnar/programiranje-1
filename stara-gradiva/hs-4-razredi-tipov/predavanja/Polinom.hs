import Data.List

-- Seštej dva seznama števil, pri čemer doda ničle na konec krajšega seznama.
sestejKoef :: (Num a) => [a] -> [a] -> [a]
sestejKoef xs [] = xs
sestejKoef [] ys = ys
sestejKoef (x:xs) (y:ys) = (x + y):(sestejKoef xs ys)

-- Pomnoži seznam števil z danim številom.
pomnoziKoef :: (Num a) => a -> [a] -> [a]
pomnoziKoef a = map (a *)

-- Izračunaj koeficiente zmnožka polinomov
zmnoziKoef :: (Num a) => [a] -> [a] -> [a]
zmnoziKoef xs [] = []
zmnoziKoef [] ys = []
zmnoziKoef (x:xs) (y:ys) = (x * y) : pomnoziKoef x ys `sestejKoef` (xs `zmnoziKoef` (y:ys))

-- Podatkovni tip polinomov
data Polinom a = Polinom [a] deriving (Eq, Ord)

-- Iz seznama koeficientov naredi polinom. Odvečne ničle (pri visokih potencah) odstrani.
izKoeficientov :: [a] -> Polinom a
izKoeficientov xs = Polinom xs

koeficienti :: Polinom a -> [a]
koeficienti (Polinom xs) = xs

-- Spremenljivka 'x'
x :: (Num a) => Polinom a
x = Polinom [0, 1]

-- Vsota dveh polinomov
sestej :: (Eq a, Num a) => Polinom a -> Polinom a -> Polinom a
sestej (Polinom xs) (Polinom ys) = izKoeficientov $ sestejKoef xs ys

-- Zmnožek dveh polinomov
zmnozi :: (Eq a, Num a) => Polinom a -> Polinom a -> Polinom a
zmnozi (Polinom xs) (Polinom ys) = izKoeficientov $ zmnoziKoef xs ys

-- Pretvorba polinomov v 'String'. To bo lahko še izboljšali, ker nepravilno
-- prikazujemo negativne koeficiente ter po nepotrebnem prikazujemo koeficient 1.
instance (Ord a, Num a, Show a) => Show (Polinom a) where
    show (Polinom []) = "0"
    show (Polinom cs) = intercalate " + " $ map showMonom $ filter ((/=0).fst) $ zip cs [0..]
        where
            showMonom (c, k) = show c ++ " " ++ showPower k
            showPower 0 = ""
            showPower 1 = "x"
            showPower k = "x^" ++ show k



-- Polinomi tvorijo kolobar in so primerek razreda 'Num'
instance (Eq a, Num a) => Num (Polinom a) where
    negate (Polinom xs) = izKoeficientov $ pomnoziKoef (-1) xs
    (+) = sestej
    (*) = zmnozi
    fromInteger x = izKoeficientov [fromInteger x]
    abs = undefined
    signum = undefined

odvod :: (Enum a, Num a) => Polinom a -> Polinom a
odvod (Polinom []) = Polinom []
odvod (Polinom (_:koef)) = Polinom $ zipWith (*) koef [1..]

integral :: (Enum a, Fractional a) => Polinom a -> Polinom a
integral p = Polinom (0 : zipWith (/) (koeficienti p) [1..])

izracunaj :: (Num a) => Polinom a -> a -> a
izracunaj (Polinom []) x = 0
izracunaj (Polinom koef) x = foldr1 (\y acc -> y + acc * x) koef

eksponentna = 1 + integral eksponentna

sinus = integral kosinus
kosinus  = 1 - integral sinus

-- to ne bo delalo, ker definicija ni generativna
eksponentna' = odvod eksponentna'
sinus' = -odvod kosinus'
kosinus' = odvod sinus'

-- tudi zgornje funkcije ne bi delale, če bi integral definirali tako, saj bi
-- funkcija najprej morala pogledati p, preden bi generirala prosti koeficient
integral' (Polinom p) = Polinom (0 : zipWith (/) p [1..])
