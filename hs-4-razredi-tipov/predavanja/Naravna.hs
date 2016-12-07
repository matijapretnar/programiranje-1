data Naravno = Nic | Nasl Naravno deriving Show

-- class Prikazljiv a where
--     pretvoriVNiz :: a -> String

-- instance  Prikazljiv Naravno  where
--     pretvoriVNiz Nic = "0"
--     pretvoriVNiz (Nasl n) = (pretvoriVNiz n) ++ "+"

instance  Num Naravno  where
    Nic + n = n
    (Nasl m) + n = Nasl (m + n)
    Nic * _ = Nic
    (Nasl m) * n = m * n + n
    fromInteger 0 = Nic
    fromInteger m = Nasl (fromInteger (m - 1))
