import qualified Slovar

type Imenik = Slovar.Slovar String Integer

prazen :: Imenik
prazen = Slovar.prazen

dodajStevilko :: Imenik -> String -> Integer -> Imenik
dodajStevilko = Slovar.dodaj

poisciStevilko :: Imenik -> String -> String
poisciStevilko imenik ime =
    case Slovar.poisci imenik ime of
        Just stevilka -> "Številka od " ++ ime ++ " je " ++ show stevilka
        Nothing -> ime ++ " nima številke"
