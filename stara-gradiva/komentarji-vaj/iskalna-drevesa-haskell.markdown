## Iskalna drevesa v Haskellu

### `izbrisi`

> V modul Slovar s prejšnjih vaj dodajte še funkcijo izbrisi,
> ki jo lahko napišete:
> * tako, kot smo jo na predavanjih ali pa
> * bolj učinkovito.

No, take rešitve, kot sem jo pokazal na predavanjih, ne bom kazal. Bom raje bolj učinkovito.

    izbrisi :: Ord a => Slovar a b -> a -> Slovar a b
    izbrisi Prazno _ = Prazno
    izbrisi (Sestavljeno l k v d) k'
        | k' < k = Sestavljeno (izbrisi l k') k v d
        | k' > k = Sestavljeno l k v (izbrisi d k')
        | otherwise = izbrisiKoren (Sestavljeno l k v d)
    
    izbrisiKoren :: (Ord a) => Slovar a b -> Slovar a b
    izbrisiKoren (Sestavljeno Prazno k v d) = d
    izbrisiKoren (Sestavljeno l k v Prazno) = l
    izbrisiKoren (Sestavljeno l _ _ d) =
        let (k', v', d') = odstraniNajboljLevega d in
        Sestavljeno l k' v' d'
    
    odstraniNajboljLevega :: Slovar a b -> (a, b, Slovar a b)
    odstraniNajboljLevega (Sestavljeno Prazno k v d) = (k, v, d)
    odstraniNajboljLevega (Sestavljeno l k v d) =
        let (k', v', l') = odstraniNajboljLevega l in
        (k', v', Sestavljeno l' k v d )

Funkcija `izbrisi` je taka kot na predavanjih (le da dela za slovarje, ne množice), `izbrisiKoren` pa je malo drugačna. Namesto, da najprej poiščemo najbolj levi element, nato pa ga še izbrišemo, bomo napisali funkcijo `odstraniNajboljLevega`, ki sprejme slovar, nato pa vrne najbolj levi ključ, pripadajočo vrednost in slovar brez tega ključa. Definicija je precej direktna, tako da je ne bi posebej razlagal, če ne bo dodatnih vprašanj.

Kot sem gledal, ste skoraj vsi oddali le rešitve nalog v Pythonu, zato težko komentiram vaše rešitve. Od dveh oddanih rešitev je bila ena taka, kot na predavanjih, druga pa različica zgornje, le da je pisalo:

    odstraniNajboljLevega (Sestavljeno l _ _ _) =
        let (k', v', l') = odstraniNajboljLevega l in
        (k', v', l')

To je narobe, ker tako pobrišemo vsa desna poddrevesa razen zadnjega na poti do najbolj levega elementa.

