import Slovar
import Test.QuickCheck

s = prazen

dodajCrke :: Slovar Char Integer -> String -> Slovar Char Integer
dodajCrke d [] = d
dodajCrke d (crka:vrstica) =
    case poisci d crka of
        Nothing -> dodajCrke (dodaj d crka 1) vrstica
        Just n -> dodajCrke (dodaj d crka (n + 1)) vrstica


dodajaj d =
    do
        vrstica <- getLine
        if null vrstica then
            return d
        else
            dodajaj $ dodajCrke d vrstica

main = do
    print (dodajCrke moj "abc")
    quickCheck (\d -> poisci (dodajCrke d "abc") 'a' == Just 1)
    --d <- dodajaj prazen
    --print d
