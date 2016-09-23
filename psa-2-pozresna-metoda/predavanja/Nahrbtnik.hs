import Data.List
import Data.Ratio

type Zaklad = [(String, Rational, Rational)]

vrednost :: Zaklad -> Rational
vrednost = sum . map (\(_, cena, _) -> cena)

enostavniNahrbtnik :: Rational -> Zaklad -> Zaklad
enostavniNahrbtnik maxTeza =
    fst . foldr dodaj ([], maxTeza) . sortOn gostota
    where
        gostota (_, cena, teza) = cena / teza

        dodaj _ (nahrbtnik, 0) = (nahrbtnik, 0)

        dodaj predmet@(ime, cena, teza) (nahrbtnik, maxTeza)
            | teza <= maxTeza = (predmet : nahrbtnik, maxTeza - teza)
            | otherwise =
                (predmet' : nahrbtnik, 0)
                  where
                    predmet' = (ime', delez * cena, delez * teza)
                    delez = maxTeza / teza
                    ime' = "(" ++ show delez ++ ") " ++ ime

-- Rešitev deluje podobno kot prej, le da prevelikih predmetov ne da
-- v nahrbtnik, temveč nadaljuje z manjšimi.
napacni01Nahrbtnik :: Rational -> Zaklad -> Zaklad
napacni01Nahrbtnik maxTeza =
    fst . foldr dodaj ([], maxTeza) . sortOn gostota
    where
        gostota (_, cena, teza) = cena / teza

        dodaj _ (nahrbtnik, 0) = (nahrbtnik, 0)

        dodaj predmet@(ime, cena, teza) (nahrbtnik, maxTeza)
            | teza <= maxTeza = (predmet : nahrbtnik, maxTeza - teza)
            | otherwise = (nahrbtnik, maxTeza)


mojZaklad :: Zaklad
mojZaklad =
    [ ("zlato", 1000, 1)
    , ("diamanti", 10000, 1)
    , ("kruh", 1, 1)
    ]

protiprimer01 :: Zaklad
protiprimer01 =
    [ ("prenosnik", 1000, 3),
      ("telefon 1", 800, 2),
      ("telefon 2", 800, 2)]

main = do
    print $ enostavniNahrbtnik 1.5 mojZaklad
    -- sledeči primer ne da pravega rezultata
    print $ napacni01Nahrbtnik 3 protiprimer01
