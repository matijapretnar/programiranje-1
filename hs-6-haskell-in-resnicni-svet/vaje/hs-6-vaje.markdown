    module Slovar
        ( Slovar
        , prazen
        , poisci
        , dodaj
        , testi1
        , testi2
        ) where
    
    import Test.QuickCheck
    import Control.Monad
    import System.Random
    
    
    --
    -- Slovar bomo implementirali z iskalnim drevesom.
    --
    data Slovar a b = Prazno
                   | Sestavljeno (Slovar a b) a b (Slovar a b)
                   deriving (Eq, Show)

Upam, da vam je zgornja definicija tipa že domača. Če ni, vprašajte na forumu, pa bom natančneje razložil. Morda bi bilo lepše, če bi namesto `a` in `b` pisali `k` in `v`, da se takoj vidi, da sta to tipa ključev in vrednosti.

### `Arbitrary (Slovar a b)`

    --
    -- Tole naredi naključno drevo.
    --
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Slovar a b) where
      arbitrary = sized tree'
        where
          tree' 0 = return Prazno
          tree' n | n > 0 =
            oneof [return Prazno,
                  liftM4 Sestavljeno subtree arbitrary arbitrary subtree]
            where subtree = tree' (n `div` 2)

Te definicije ne bom razlagal. Kogar bolj zanima, naj si pogleda dokumentacijo paketa QuickCheck.
    
### Osnovne funkcije na dvojiških drevesih in njihove lastnosti

    -- Sestavite funkcijo prezrcali, ki med seboj zamenja levo in desno
    -- poddrevo (v vseh vozliščih).
    
    prezrcali :: Slovar a b -> Slovar a b
    prezrcali Prazno = Prazno
    prezrcali (Sestavljeno l k v d) = Sestavljeno (prezrcali d) k v (prezrcali l)
    
    -- Sestavite funkcijo globina, ki vrne globino drevesa.
    
    globina :: Slovar a b -> Int
    globina Prazno = 0
    globina (Sestavljeno l _ _ d) = 1 + max (globina l) (globina d)
    
    -- Če drevo dvakrat prezrcalimo, dobimo spet isto nazaj. Napišite ustrezno
    -- lastnost prop_prezrcaliPrezrcali.
    
    prop_prezrcaliPrezrcali :: (Eq a, Eq b) => Slovar a b -> Bool
    prop_prezrcaliPrezrcali s = prezrcali (prezrcali s) == s
    
    -- Če drevo prezrcalimo, se mu globina pri tem ne spremeni. Napišite ustrezno
    -- lastnost prop_globinaPrezrcali.
    
    prop_globinaPrezrcali :: (Eq a, Eq b) => Slovar a b -> Bool
    prop_globinaPrezrcali s = globina (prezrcali s) == globina s
    
    testi1 = do
        quickCheck (prop_prezrcaliPrezrcali :: Slovar Int Int -> Bool)
        quickCheck (prop_prezrcaliPrezrcali :: Slovar Int Char -> Bool)
        quickCheck (prop_globinaPrezrcali :: Slovar Int Int -> Bool)
        quickCheck (prop_globinaPrezrcali :: Slovar Int Char -> Bool)

Nobena od teh nalog vam ni povzročala težav, tako da posebnih komentarjev nimam.

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

### Osnovne funkcije na iskalnih drevesih

    -- Definirajte prazen slovar.
    
    prazen :: Slovar a b
    prazen = Prazno
    
    -- Definirajte metodo poisci, ki v slovarju poišče vrednost danega ključa.
    
    poisci :: (Ord a) => Slovar a b -> a -> Maybe b
    poisci Prazno _ = Nothing
    poisci (Sestavljeno l k v d) k'
       | k' < k = poisci l k'
       | k' > k = poisci d k'
       | otherwise = Just v
    
    -- Definirajte metodo dodaj, ki v slovar doda podan ključ in vrednost.
    
    dodaj :: (Ord a) => Slovar a b -> a -> b -> Slovar a b
    dodaj Prazno k v = Sestavljeno Prazno k v Prazno
    dodaj (Sestavljeno l k v d) k' v'
        | k' < k = Sestavljeno (dodaj l k' v') k v d
        | k' > k = Sestavljeno l k v (dodaj d k' v')
        | otherwise = Sestavljeno l k v' d

Glede na to, da gre tukaj samo za manjšo različico kode s predavanj, večjih težav ni bilo.

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
