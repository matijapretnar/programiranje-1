import Data.List
import Data.Ratio

type Predmet = (String, Rational, Rational)
type Zaklad = [Predmet]


gostota :: Predmet -> Rational
gostota (_, cena, teza) = cena / teza

vrednost :: Zaklad -> Rational
vrednost = sum . map (\(_, cena, _) -> cena)

teza :: Zaklad -> Rational
teza = sum . map (\(_, _, t) -> t)

enostavniNahrbtnik :: Rational -> Zaklad -> Zaklad
enostavniNahrbtnik maxTeza =
    fst . foldr dodaj ([], maxTeza) . sortOn gostota
    where
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
        dodaj _ (nahrbtnik, 0) = (nahrbtnik, 0)
        dodaj predmet@(ime, cena, teza) (nahrbtnik, maxTeza)
            | teza <= maxTeza = (predmet : nahrbtnik, maxTeza - teza)
            | otherwise = (nahrbtnik, maxTeza)


pravi01Nahrbtnik :: Rational -> Zaklad -> Zaklad
pravi01Nahrbtnik maxTeza predmeti = snd $ najboljsaMoznost maxTeza predmeti
  where
    najboljsaMoznost _ [] = (0, [])
    najboljsaMoznost 0 _ = (0, [])
    najboljsaMoznost maxTeza (predmet@(_, cena, teza):predmeti)
        | teza <= maxTeza =
            let (cenaBrez, predmetiBrez) = najboljsaMoznost maxTeza predmeti
                (cenaZ', predmetiZ') = najboljsaMoznost (maxTeza - teza) predmeti
                (cenaZ, predmetiZ) = (cena + cenaZ', predmet:predmetiZ') in
            max (cenaZ, predmetiZ) (cenaBrez, predmetiBrez)
        | otherwise = najboljsaMoznost maxTeza predmeti


hitri01Nahrbtnik :: Rational -> Zaklad -> Zaklad
hitri01Nahrbtnik maxTeza predmeti = snd $ najboljsaMoznost maxTeza $ sortOn gostota $ predmeti
  where
    najboljsaMoznost _ [] = (0, [])
    najboljsaMoznost 0 _ = (0, [])
    najboljsaMoznost maxTeza (predmet@(_, cena, teza):predmeti)
        | teza <= maxTeza =
            let ocenaBrez = vrednost (enostavniNahrbtnik maxTeza predmeti)
                ocenaZ = cena + vrednost (enostavniNahrbtnik (maxTeza - teza) predmeti)
                (cenaBrez, predmetiBrez) = najboljsaMoznost maxTeza predmeti
                (cenaZ', predmetiZ') = najboljsaMoznost (maxTeza - teza) predmeti
                (cenaZ, predmetiZ) = (cena + cenaZ', predmet:predmetiZ') in
            if ocenaZ <= ocenaBrez && ocenaZ <= cenaBrez then
                (cenaBrez, predmetiBrez)
            else if ocenaBrez <= ocenaZ && ocenaBrez <= cenaZ then
                (cenaZ, predmetiZ)
            else
                max (cenaZ, predmetiZ) (cenaBrez, predmetiBrez)
        | otherwise = najboljsaMoznost maxTeza predmeti


protiprimer01 :: Zaklad
protiprimer01 =
    [ ("prenosnik", 1000, 3),
      ("telefon 1", 800, 2),
      ("telefon 2", 800, 2)]

vecjiPrimer :: Integer -> Zaklad
vecjiPrimer m = [("predmet " ++ show n, ((10 * n) `mod` 19 + 1) % 1, ((10 * n) `mod` 23 + 1) % 1) | n <- [1..m]]

main = do
    print $ teza $ enostavniNahrbtnik 60 (vecjiPrimer 15)
    print $ teza $ napacni01Nahrbtnik 60 (vecjiPrimer 15)
    print $ teza $ pravi01Nahrbtnik 60 (vecjiPrimer 15)
    print $ teza $ hitri01Nahrbtnik 60 (vecjiPrimer 15)
