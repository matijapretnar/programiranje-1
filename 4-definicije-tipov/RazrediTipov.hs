data Barva = Rdeca
           | Rumena
           | Modra
           | Bela
           | Crna
           | Siva
           | Druga String
           deriving (Eq, Ord)

imeBarve :: Barva -> String
imeBarve Rdeca = "rdeca"
imeBarve Rumena = "rumena"
imeBarve Modra = "modra"
imeBarve Bela = "bela"
imeBarve Crna = "crna"
imeBarve Siva = "siva"
imeBarve (Druga ime) = ime

instance Show Barva where
    show = imeBarve

-- class Show a where
--     show :: a -> String

data Naravna = Nic | Naslednik Naravna deriving Show

instance Num Naravna where
    a + Nic = a
    a + Naslednik b = Naslednik (a + b)
    
    a * Nic = Nic
    a * Naslednik b = a + a * b
    
    abs n = n

    signum Nic = Nic
    signum (Naslednik _) = Naslednik Nic

    fromInteger 0 = Nic
    fromInteger n | n > 0 = Naslednik $ fromInteger $ n - 1
