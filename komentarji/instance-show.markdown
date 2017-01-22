### `instance Show Kompleksno`

Navodila so pisala:

>     -- Kompleksna števila naj bodo izpisana v takšni obliki: 3 + 5i

zato ste pisali

    instance Show Kompleksno where
        show (Kompleksno x y) = show x ++ " + " ++ show y ++ "i"

ali pa celo bolj natančno kot

    instance Show Kompleksno where
        show (Kompleksno 0 0) = "0"
        show (Kompleksno x 0) = show x
        show (Kompleksno 0 y) = show y ++ "i"
        show (Kompleksno x y)
            | y < 0 = show x ++ " - " ++ show (-y) ++ "i"
            | y > 0 = show x ++ " + " ++ show y ++ "i"

(zakaj v zadnjem primeru pogoj `y = 0` ni potreben?) V resnici pa je dogovor, da funkcija `show` tip prikaže v obliki, ki je berljiva tudi Haskellu. Torej bi navodila morala biti:

>     -- Kompleksna števila naj bodo izpisana v takšni obliki: Kompleksno 3 5

kar bi lahko spet dosegli z `deriving Show`. Mogoče vas na tej točki zanima, kaj je potem sploh namen tega, da sami pišemo funkcijo `show`. Včasih je interna oblika človeku res neprebavljiva. Zadnjič smo na predavanjih množice predstavili z iskalnimi drevesi kot:

    data Mnozica a = Prazna | Sestavljena (Mnozica a) a (Mnozica a)

Kot smo videli, je predstavitev z iskalnimi drevesi odlična, vendar nočemo, da bi nam računalnik množico `{1, 2, 3, 4}` prikazal kot

    Sestavljena (Sestavljena Prazna 1 Prazna) 2 (Sestavljena Prazna 3 (Sestavljena Prazna 4 Prazna))

Kako stvar naredimo berljivo tako nam kot računalniku? Na podobno težavo naletimo, kadar hočemo implementacijo skriti – na primer nočemo pokazati, da so množice implementirane z iskalnimi drevesi. Rešitev je, da najprej napišemo pomožno funkcijo `fromList`, ki vrednost sestavi iz seznama

    fromList :: [a] -> Mnozica a
    fromList = foldl dodaj Prazna

(razumevanje delovanja vam prepuščam za vajo). Nato lahko napišemo

    instance Show a => Show (Mnozica a) where
        show m = "fromList " ++ show (toList m)
          where
            toList Prazna = []
            toList (Sestavljena l x d) = toList l ++ [x] ++ toList d

in množica `{1, 2, 3, 4}` se bo prikazala kot `fromList [1,2,3,4]`, kar je razumljivo tako človeku kot računalniku.





### `instance Show Polinom`

Najprej si oglejmo enostavni primer, ko so koeficienti celoštevilski. Tudi tu bi lahko polinome prikazali v obliki, ki jo razume računalnik, na primer `Polinom [1, 2, -1]`. Toda ker smo si že definirali polinom `x = Polinom [0, 1]`, s katerim lahko računamo kot bi pričakovali, lahko tudi polinome prikažemo kot `1 + 2 * x - x^2`. Pri lepem prikazu je potrebno paziti na več stvari:

* ne pišemo `3 * x^0 + 2 * x^1 + 4 * x^2` temveč `3 + 2 * x + 4 * x^2`
* ne pišemo `1 + 0 * x + 2 * x^2` temveč `1 + 2 * x^2`
* ne pišemo `1 + -2 * x` temveč `1 - 2 * x`
* vseeno pišemo `-2 * x + 3 * x^2` namesto `- 2 * x + 3 * x^2`
* ne pišemo `2 + 1 * x` ali `2 - 1 * x` temveč `2 + x` ali `2 - x`

Če upoštevamo vse to, lahko napišemo:

    instance Show Polinom where
        show (Polinom koef) = prikazi $ filter ((/= 0) . fst) $ zip koef [0..]
            where
                prikazi [] = "0"
                prikazi ((a, 0):monomi) = foldl dodaj (show a) monomi
                prikazi ((a, n):monomi) = foldl dodaj (koeficient a ++ potenca n) monomi
                dodaj niz (a, n)
                    | a > 0 = niz ++ " + " ++ koeficient a ++ potenca n
                    | a < 0 = niz ++ " - " ++ koeficient (-a) ++ potenca n
                koeficient 1 = ""
                koeficient a = show a ++ " * "
                potenca 0 = ""
                potenca 1 = "x"
                potenca n = "x^" ++ show n

Ista koda deluje tudi za racionalne koeficiente, vendar je težava, da izpisa Haskell ne more prebaviti. Zakaj? Če Haskellu podamo `3 * x`, bo vedel, da naj `3` pretvori v `Polinom [3]`, ker smo povedali, da `Polinom` pripada razredu `Num`, torej ima tudi funkcijo `fromInteger`. Če bi želeli podobno doseči za racionalna števila, bi morali pokazati, da `Polinom` pripada razredu `Fractional`, kar pa poleg funkcije `fromRational :: Rational -> Polinom` zahteva tudi deljenje `(/) :: Polinom -> Polinom -> Polinom`. To pa s polinomi ne gre. V tem primeru je najbolj varno napisati

    instance Show Polinom where
        show (Polinom koef) = "polinom " ++ show koef

Pozorni bodite na to, da sem napisal `"polinom "` namesto `"Polinom "`, da v izpisu ponudim pametni konstruktor.


    import Data.Ratio -- Od tu dobimo racionalna števila
    import Data.List (sort, maximumBy) -- Od tukaj smo dobili sortBy.
    import Data.Function (on) -- Od tu dobimo on

Zdi se mi, da je bilo v drugi vrstici mišljeno, da bi uvozili `sortBy` namesto `sort`. Ko smo že pri uvažanju, namesto `on` lahko uvozimo tudi `comparing`, h kateremu se bomo kmalu vrnili.

    import Data.Ord (comparing)
