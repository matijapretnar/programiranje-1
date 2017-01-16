### Podatkovni tip `Drevo`

>     -- Spodaj je definiran rekurzivni podatkovni tip Drevo. Dodali bomo še nekaj funkcij
>     -- za delo s tipom Drevo. Kot zgled je že definirana funkcija vsota, ki izračuna vsoto
>     -- vseh elementov v drevesu.
> 
>     data Drevo a = Prazno | Sestavljeno a (Drevo a) (Drevo a)
> 
>     vsota :: Num a => Drevo a -> a
>     vsota Prazno = 0
>     vsota (Sestavljeno x levo desno) = x + vsota levo + vsota desno
> 
>     instance (Show a) => Show (Drevo a) where
>         show Prazno = "Prazno"
>         show (Sestavljeno x Prazno Prazno) = "Sestavljeno " ++ show x ++ " Prazno Prazno"
>         show (Sestavljeno x Prazno desno) = "Sestavljeno " ++ show x ++ " Prazno (" ++ show desno ++ ")"
>         show (Sestavljeno x levo Prazno) = "Sestavljeno " ++ show x ++ " (" ++ show levo ++ ") Prazno"
>         show (Sestavljeno x levo desno) = "Sestavljeno " ++ show x ++ " (" ++ show levo ++ ") (" ++ show desno ++ ")"

Tu vam je asistent že prijazno napisal funkcijo `show`. Če bi bil malo bolj len, bi lahko isto dosegel tudi s tem, da bi za definicijo tipa `Drevo` dodal še `deriving Show`.

### `globina`

>     -- Sestavite funkcijo globina, ki vrne globino drevesa. Prazno drevo ima globino 0.
>     -- 
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> globina d
>     -- 3

Tu ste praktično vsi napisali

    globina :: Drevo a -> Integer
    globina Prazno = 0
    globina (Sestavljeno _ levo desno) = 1 + max (globina levo) (globina desno)

Kdo je dodal kakšen oklepaj preveč, kdo je pozabil dodati signaturo, kdo pa je namesto `_` v vzorcu neuporabljeni spremenljivki dal ime. Nič groznega, ampak vseeno tega ne počnite. Nekateri ste od signature funkcije `vsota` prekopirali tudi `Num a`, ki pa je tokrat nepotreben.

### `steviloElementov`

>     -- Sestavite funkcijo steviloElementov, ki vrne število elementov v drevesu.
>     -- 
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> steviloElementov d
>     -- 4 

Tudi tu ste vsi (s podobnimi napakicami kot zgoraj) napisali:

    steviloElementov :: Drevo a -> Integer
    steviloElementov Prazno = 0
    steviloElementov (Sestavljeno _ levo desno) = 1 + steviloElementov levo + steviloElementov desno

### `prezrcali`

>     -- Sestavite funkcijo prezrcali, ki drevo prezrcali, tako da pri vsakem vozlišču zamenja levo in desno poddrevo.
>     -- 
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> prezrcali d
>     -- Sestavljeno 3 (Sestavljeno 8 Prazno Prazno) (Sestavljeno 7 (Sestavljeno 2 Prazno Prazno) Prazno)

In tudi tu ste vsi (spet s podobnimi napakicami kot zgoraj) napisali:

    prezrcali :: Drevo a -> Drevo a
    prezrcali Prazno = Prazno
    prezrcali (Sestavljeno x levo desno) = Sestavljeno x (prezrcali desno) (prezrcali levo)

### `najboljLevi`

Ne vem, kaj se je tu zgodilo z navodili:

>     -- Sestavite funkcijo, ki drevo prezrcali, tako da levo postane desno, desno pa levo.
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> prezrcali d
>     -- Sestavljeno 3 (Sestavljeno 8 Prazno Prazno) (Sestavljeno 7 (Sestavljeno 2 Prazno Prazno) Prazno)

Verjetno je bilo mišljeno nekaj v stilu:

>     -- Sestavite funkcijo, ki vrne najbolj levi element v drevesu.
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> najboljLevi d
>     -- 7

kar bi naredili z:
     
    najboljLevi :: Drevo a -> a
    najboljLevi (Sestavljeno x Prazno _) = x
    najboljLevi (Sestavljeno _ levo __) = najboljLevi levo 

Za to, kaj narediti s praznim drevesom, obstaja več možnosti. Lahko preprosto ne naštejete primera, lahko pa eksplicitno javite napako

    najboljLevi Prazno = error "najboljLevi: prazno drevo"

Običajno se napake piše v obliki `"ime funkcije: opis napake"`, poizkusite na primer `tail []`.

Ena izmed predlaganih rešitev je bila tudi

    najboljLevi Prazno = 0

kar pa ni dobro. Dobro je, da napake, takrat ko se zgodijo, to storijo na najbolj glasen način. Skrite napake so najhujše, saj jih ponavadi odkrijemo prepozno. Še en poskus rešitve, ki sem ga videl, je bil

    najboljLevi Prazno = Prazno

kar pa seveda zaradi tipov nima smisla.

Seveda bi bilo najbolje, če bi asistent primer v navodilih napisal kot

>     -- ghci> najboljLevi d
>     -- Just 7

V tem primeru ni nikakršnega dvoma, kaj napisati:

    najboljLevi :: Drevo a -> Maybe a
    najboljLevi Prazno = Nothing
    najboljLevi (Sestavljeno x Prazno _) = Just x
    najboljLevi (Sestavljeno _ levo __) = najboljLevi levo 

### Podatkovni tip `Kompleksno`

>     -- Definiran je podatkovni tip Kompleksno, ki predstavlja kompleksno število. Dodali bomo še nekaj funkcij
>     -- za delo s kompleksnimi števili.
> 
>     data Kompleksno = Kompleksno Double Double

### `re`, `im` in `konjugiraj`

Pravilne rešitve bi (in večinoma tudi so) bile

    re, im :: Kompleksno -> Double
    re (Kompleksno x _) = x
    im (Kompleksno _ y) = y

    konjugiraj :: Kompleksno -> Kompleksno
    konjugiraj (Kompleksno x y) = Kompleksno x (-y)

(Ste videli, kako lahko za dve funkciji z istim tipom hkrati povemo signaturo? Ravno prava stvar za tiste bolj lenobne med vami.)

### `instance Num Kompleksno`

Tudi tu so vam stvari načeloma šle (z izjemo par računskih spodrsljajev, ki niso moja odgovornost, vas bom pa zatožil prof. Černetu):

    instance Num Kompleksno where
        (Kompleksno x1 y1) + (Kompleksno x2 y2) = Kompleksno (x1 + x2) (y1 + y2)
        (Kompleksno x1 y1) * (Kompleksno x2 y2) = Kompleksno (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)
        abs (Kompleksno x y) = Kompleksno (sqrt (x^2 + y^2)) 0
        fromInteger n = Kompleksno (fromInteger n) 0
        negate (Kompleksno x y) = Kompleksno (negate x) (negate y)
        signum (Kompleksno 0 0) = Kompleksno 0 0
        signum (Kompleksno x y) = Kompleksno (x / abs) (y / abs)
          where
            abs = sqrt (x^2 + y^2)

Nekateri ste mnenja, da kompleksna števila nimajo predznaka, in ste pisali

    signum _ = error "signum: kompleksna števila nimajo predznaka"

ali kaj podobnega. Tudi to je, kar se me tiče, v redu, saj navodila glede tega niso bila natančna. Bomo pa videli, kaj bo porekel prof. Černe…

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

### Podatkovni tip `Polinom`

>     -- Definiran je podatkovni tip Polinom, ki predstavlja polinom na kolobarjem celih števil. Dodali bomo še
>     -- nekaj funkcij za delo s polinomi.
> 
>     data Polinom = Polinom [Integer]
> 
>     x :: Polinom
>     x = Polinom [0, 1]     

Kot ste ugotovili na vajah, je celoštevilske polinome malo težje integrirati, zato raje uporabljajmo racionalna števila.

    import Data.Ratio
    data Polinom = Polinom [Rational]

Praktično je imeti tudi pametni konstruktor, ki s konca pobriše vse ničelne koeficiente, in destruktor, ki vrne seznam koeficientov:

    polinom :: [Rational] -> Polinom
    polinom = Polinom . reverse . dropWhile (== 0) . reverse
    
    koeficienti :: Polinom -> [Rational]
    koeficienti (Polinom koef) = koef

Če želimo, lahko funkciji poimenujemo tudi kot `fromList` in `toList`.

Funkcija `polinom` dela v linearnem času. Nekateri ste pisali

    polinom sez
        | last sez == 0 = polinom $ init sez
        | otherwise = Polinom sez

Ta se v vsakem koraku zapelje čez ves seznam, zato deluje v kvadratnem času. Pa tudi napaka se zgodi, če v seznamu ni neničelnega števila. Sicer pa je tudi to bolje kot

    polinom = takeWhile (/= 0)

ki res deluje v linearnem času in ne javlja napake, vendar potihem dela napačno (zakaj, ugotovite sami).

### `odvod`

Večina vas je ugotovila, da se odvod enostavno izračuna s pomočjo `zip` in seznama `[1..]`, paziti moramo le na prazen seznam koeficientov. Z `zipWith` je rešitev še malo bolj elegantna:

    odvod :: Polinom -> Polinom
    odvod (Polinom []) = polinom []
    odvod (Polinom (_:koef)) = polinom $ zipWith (*) koef [1..]

### `integral`

Tudi tu je rešitev podobna (če delate z racionalnimi števili, seveda :)

    integral :: Polinom -> Polinom
    integral (Polinom koef) = polinom $ 0 : zipWith (/) koef [1..]

### `eval`

Najbolj neposredno lahko vrednost izračunamo kot

    eval :: Polinom -> Rational -> Rational
    eval (Polinom koef) x = sum $ zipWith (\k n -> k * x^n) koef [0..]

Če rešitev pišete kot

    eval (Polinom koef) x = sum $ map (\n -> (koef !! n) * x^n) [0..]

pa to ni najbolj učinkovito, saj gre `(!!)` ustrezni koeficient vsakič iskati od začetka seznama.

Lahko pa uporabimo tudi Hornerjev algoritem:
$$a_0 + a_1 x + a_2 x^2 + \dots + a_n x^n
= a_0 + x (a_1 + x (a_2 + \cdots + x (a_{n - 1} + x a_n)) \cdots ).$$
Začnemo torej z akumulatorjem $a_n$, ga ob vsakem koraku pomnožimo z $x$ in prištejemo koeficient, pri čemer gremo od desne proti levi. Algoritem torej enostavno implementiramo s `foldr1`:

    eval (Polinom koef) x = foldr1 (\a acc -> a + x * acc) koef

Namesto `foldr1` ste pisali tudi

    eval (Polinom koef) x = foldr (\a acc -> a + x * acc) (last koef) (init koef)

kar deluje enako, vendar gre prej dvakrat po nepotrebnem čez seznam, da `last` dobi njegov zadnji člen, `init` pa vse člene razen zadnjega. Pri tem `init` tudi naredi nov seznam, zato rešitev ni najbolj učinkovita.

Nekateri ste Hornerjev algoritem napisali tudi rekurzivno:

    eval (Polinom []) _ = 0
    eval (Polinom (k:koef)) x = k + x * eval (Polinom koef) x

Skoraj vsaka od teh rešitev je imela kakšno pomanjkljivost (verjetno po naključju, saj s samo rešitvijo ni sicer nič narobe):

* pozabljeni končni primer za prazen seznam koeficientov;
* odvečni primer `eval (Polinom [k]) _`, ki sledi že iz zgornjih dveh primerov;
* zadnji primer pisan kot `eval (Polinom koef) x = head koef + x * eval (Polinom (tail koef)) x` namesto z elegantnejšimi in varnejšimi vzorci.



### `instance Num Polinom`

Pri tem, da `Polinom` pripada `Num`, je bilo največ dela s tem, da ste pazili na ustrezno dolžino seznamov pri seštevanju:

    instance Num Polinom where
        negate (Polinom koef) = polinom $ pomnoziKoef (-1) koef
        (Polinom koef1) + (Polinom koef2) = polinom $ sestejKoef koef1 koef2
        (Polinom koef1) * (Polinom koef2) = polinom $ zmnoziKoef koef1 koef2
        fromInteger x = polinom $ [fromInteger x]
        abs = error "abs: polinomi nimajo definirane absolutne vrednosti"
        signum = error "abs: polinomi nimajo definiranega predznaka"

Seveda si moramo le definirati funkcije za delo s koeficienti.

    sestejKoef :: [Rational] -> [Rational] -> [Rational]
    sestejKoef xs [] = xs
    sestejKoef [] ys = ys
    sestejKoef (x:xs) (y:ys) = (x + y):(sestejKoef xs ys)

Malo manj učinkovita možnost je, da za seštevanje uporabite `zipWith (+)`, nato pa na konec dodate še preostanek daljšega seznama. Obravnavanju različnih primerov se lahko izognete tudi tako, da na konec dodate oba preostanka, saj bo eden od njiju zagotovo prazen:

    sestejKoef xs ys = vsota ++ drop n xs ++ drop n ys
      where
        vsota = zipWith (+) xs ys
        n = length vsota

Še bolj neučinkovita možnost pa je, da uporabljate `(!!)`, ampak to zdaj že veste. Pri množenju gre podobno (doma si enačbe napišite na list, da vidite, zakaj delujejo pravilno):

    pomnoziKoef :: Rational -> [Rational] -> [Rational]
    pomnoziKoef a = map (a *)

    zmnoziKoef :: [Rational] -> [Rational] -> [Rational]
    zmnoziKoef _ [] = []
    zmnoziKoef [] _ = []
    zmnoziKoef (x:xs) (y:ys) = (x * y) : pomnoziKoef x ys `sestejKoef` (xs `zmnoziKoef` (y:ys))

Ena od rešitev (tukaj malo poenostavljena) je bila tudi, da ste paroma pomnožili vse monome, nato pa jih združili po potencah:

    pomnoziKoef xs ys =
        [sum [x | (x, m) <- pari, m == n] | n <- [0..maximum (map snd pari)]]
          where
            pari = [(x * y, m + n) | (x, m) <- zip xs [0..], (y, n) <- zip ys [0..]]

Ker moramo za vsako potenco `n` preiskati ves seznam parov za ustrezne potence `m`, ta rešitev ni najbolj učinkovita. Natančneje, njena časovna zahtevnost je $O(n^3)$, kjer je $n$ večja od stopenj vhodnih polinomov. Zgornja rešitev ima časovno zahtevnost $O(n^2)$. Če vas zanima: obstajajo tudi rešitve s še manjšo časovno zahtevnostjo.


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
