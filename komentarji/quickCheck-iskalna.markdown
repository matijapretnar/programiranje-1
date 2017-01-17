### `jeIskalno`

    -- Napišite funkcijo jeIskalno, ki preveri, če je dano drevo res iskalno drevo.
    -- (Naj vam pri tem pomaga asistent.)

No, tu je bilo pa bolj veselo. Bom rekel asistentu, naj bolj pomaga. Skoraj vsi ste napisali rešitev, ki je najprej rekurzivno preverila levo in desno drevo, da sta iskalni, nato pa preiskala vse levo drevo, da je našla maksimum, in vse desno drevo, da je našla minimum, nato pa ti dve primerjala s korenom. Ker je časovna zahtevnost iskanja minimuma/maksimuma $O(n)$, kjer je $n$ število vozlišč v drevesu, in ker ste to preverjali rekurzivno, je časovna zahtevnost $O(n^2)$. Stvar bi lahko izboljšali že tako, da za minimum in maksimum ne bi preiskali celotnih poddreves, temveč le vzeli najbolj desni element v levem in najbolj levi element v desnem poddrevesu. S tem časovna zahtevnost pade na približno $O(n g)$, kjer je $g$ povprečna globina drevesa. A zadevo se da v resnici najhitreje (in z najkrajšo rešitvijo) preveriti v linearnem času:

    jeIskalno :: (Ord a) => Slovar a b -> Bool
    jeIskalno d = jeIskalno' Nothing d Nothing
      where
        jeIskalno' _ Prazno _ = True
        jeIskalno' mini (Sestavljeno l k _ d) maksi =
            jeIskalno' mini l (Just k) &&
            mini <=? Just k && Just k <=? maksi &&
            jeIskalno' (Just k) d maksi
        Nothing <=? _ = True
        _ <=? Nothing = True
        Just x <=? Just y = x <= y

Pomožna funkcija `jeIskalno' mini d maksi` preveri, če je drevo `d` iskalno in so vsi njegovi elementi med številoma `mini` in `maksi`. To preverimo tako, da pogledamo, če je koren drevesa `k` med številoma, nato pa za levo drevo preverimo, da je med `mini` in `k`, za desno pa, da je med `k` in `maksi`. Ker skrajnih vozlišč ne preverjamo, lahko za meji vzamemo tudi $\pm \infty$, kar predstavimo z `Nothing`, take vrednosti pa primerjamo s pomočjo funkcije `<=? : (Ord a) => Maybe a -> Maybe a -> Bool` (saj ste vedeli, da lahko pomožne funkcije pišemo tudi s simboli, kajne?).

### Lastnosti osnovnih funkcij na iskalnih drevesih

    --
    -- Asistent bo na pojasnil naslednje lastnosti. Generatorja
    -- iskalnaDrevesa zaenkrat še ne razumemo.
    --
    prop_jeIskalnoDodaj :: Slovar Int String -> Int -> String -> Property
    prop_jeIskalnoDodaj d k v =
        collect (globina d) $ collect (jeIskalno d) $ sledi (jeIskalno d) (jeIskalno (dodaj d k v))
        where
            sledi True False = False
            sledi _ _ = True

Torej, če asistent stvari slučajno ni pojasnil (po tistem z `jeIskalno` mu ne zaupam več najbolj :), je zadeva sledeča. Radi bi preverili, da velja `jeIskalno (dodaj d k v)`, kadar velja `jeIskalno d`. V ta namen si definiramo logično operacijo `sledi`, ki se obnaša kot implikacija. Če poženemo teste, dobimo:

    +++ OK, passed 100 tests:
    50% 0, True
    11% 6, False
    10% 1, True
     8% 7, False
     5% 5, False
     5% 2, True
     5% 2, False
     4% 4, False
     2% 3, False

V zgornjem testu smo napisali `collect (globina d)` in `collect (jeIskalno d)`, s čimer smo dosegli, da QuickCheck rezultate združi glede na vrednosti `globina d` in `jeIskalno d`. Zaradi tega se tudi končni tip iz `Bool` spremeni v splošnejši `Property`. Kot vidimo iz rezultatov, je bilo le 2% testov izvedenih na iskalnih drevesih globine 2, na iskalnih drevesih globine 3 ali več pa ni bilo nobenega testa. Poleg tega je bilo 35% testov izvedenih na neiskalnih drevesih, zato so bili trivialni. Če se želimo omejiti le na iskalna drevesa, raje uporabimo operacijo `==>`, ki jo nudi QuickCheck. Ta v primeru, da pogoj na levi velja, izvede test na desni, sicer pa poskusi najti drug primer.

    prop_jeIskalnoDodaj' :: Slovar Int String -> Int -> String -> Property
    prop_jeIskalnoDodaj' d k v =
        collect (globina d) $ collect (jeIskalno d) $ jeIskalno d ==> jeIskalno (dodaj d k v)

V tem primeru dobimo:

    +++ OK, passed 100 tests:
    80% 0, True
    15% 1, True
     4% 2, True
     1% 3, True

To je malo bolje, saj sedaj izvedemo vseh 100 testov zgolj na iskalnih drevesih, vendar še vseeno na bolj majhnih. Še boljša rešitev je, da sami napišemo generator naključnih iskalnih dreves:
    
    iskalnaDrevesa :: Gen (Slovar Int String)
    iskalnaDrevesa =
        sized $ \n -> do
            mini <- arbitrary
            maksi <- arbitrary
            tree' mini maksi n
        where
          tree' mini maksi n
            | n > 0 && mini <= maksi =
                oneof [return Prazno, do
                        k <- choose (mini, maksi)
                        v <- arbitrary
                        l <- tree' mini (pred k) (n `div` 2)
                        d <- tree' (succ k) maksi (n `div` 2)
                        return (Sestavljeno l k v d)]
            | otherwise = return Prazno

Točnega zapisa ne bom razlagal, ampak ideja je sledeča: izmislimo si naključno velikost `n` ter naključni meji `mini` in `maksi`, nato pa rekurzivno sestavimo drevo s ključem med `mini` in `maksi` ter poddrevesoma velikosti `n / 2` med ustreznimi mejami. Tedaj se lahko v testih s funkcijo `forAll` omejimo le na vrednosti, ki jih vrača ta generator:

    prop_jeIskalnoDodaj'' :: Int -> String -> Property
    prop_jeIskalnoDodaj'' k v =
        forAll iskalnaDrevesa $ \d ->
        collect (globina d) $ collect (jeIskalno d) $ jeIskalno (dodaj d k v)

Rezultati so tedaj:

    79% 0, True
     7% 1, True
     5% 2, True
     4% 4, True
     2% 7, True
     2% 5, True
     1% 3, True

Torej, vsa drevesa, ki smo jih ustvarili, so bila iskalna (kar pomeni, da naš generator deluje pravilno), pri čemer jih je bilo 10% precej velikih.

    -- Če v drevo vstavimo nov element, se mu globina poveča ali pa ostane enaka.
    
    prop_globinaDodaj :: Int -> String -> Property
    prop_globinaDodaj k v =
        forAll iskalnaDrevesa $ \d ->
        globina (dodaj d k v) >= globina d
    
    -- Če v drevo dodamo nov element, se globina poveča kvečjemu za ena.
    
    prop_globinaDodaj' :: Int -> String -> Property
    prop_globinaDodaj' k v =
        forAll iskalnaDrevesa $ \d ->
        globina (dodaj d k v) <= globina d + 1
    
    -- Če smo v slovar ravnokar vstavili nek ključ in vrednost, moramo dobiti
    -- pri iskanju tega ključa vrednost, ki smo jo ravnokar vstavili.
    
    prop_poisciDodaj :: Int -> String -> Property
    prop_poisciDodaj k v =
        forAll iskalnaDrevesa $ \d ->
        poisci (dodaj d k v) k == Just v

Tu niste imeli večjih težav (mogoče ste nekateri pogoja `prop_globinaDodaj` in `prop_globinaDodaj'` izrazili malo nerodno). Ste pa skoraj vsi v testih obdržali `collect (globina d)` in `collect (jeIskalno d)`, kar tu ni imelo večjega smisla, sploh drugo, ker generiramo le iskalna drevesa. To ni nič groznega, rad bi le, da se zavedate, za kaj uporabljamo `collect`: za to, da izvemo koliko testov se je izvedlo pri kakšnih vrstah primerov.

    -- Če vstavimo nek ključ in vrednost v in k ter takoj zatem že nek par
    -- v' in k', kjer je v' različna od v, potem moramo pri iskanju vrednosti
    -- od ključa k dobiti v.
    
    prop_poisciDodaj2 :: Int -> String -> Int -> String -> Property
    prop_poisciDodaj2 k v k' v' =
        forAll iskalnaDrevesa $ \d ->
        k /= k' ==> poisci (dodaj (dodaj d k v) k' v') k == Just v

Tudi ta del vam je šel. V tem primeru `==>` dobro služi svojemu namenu, saj velika večina naključno izbranih vzorcev zadošča pogoju `k /= k'` (za razliko od prej, kjer je bila verjetnost, da bo naključno izbrano drevo iskalno, majhna).
    
    testi2 = do
        quickCheck prop_jeIskalnoDodaj
        quickCheck prop_jeIskalnoDodaj'
        quickCheck prop_jeIskalnoDodaj''
        quickCheck prop_globinaDodaj
        quickCheck prop_globinaDodaj'
        quickCheck prop_poisciDodaj
        quickCheck prop_poisciDodaj2
