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

>     -- Sestavite funkcijo, ki vrne najbolj levi element v drevesu.
>     -- Zgled:
>     -- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
>     -- ghci> najboljLevi d
>     -- 7

Rešitev je:
     
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


### Podatkovni tip `Polinom`

>     -- Definiran je podatkovni tip Polinom, ki predstavlja polinom nad obsegom racionalnih števil. Dodali bomo še
>     -- nekaj funkcij za delo s polinomi.
> 
>     data Polinom = Polinom [Rational]
> 
>     x :: Polinom
>     x = Polinom [0, 1]     

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

### `odvod`

Večina vas je ugotovila, da se odvod enostavno izračuna s pomočjo `zip` in seznama `[1..]`, paziti moramo le na prazen seznam koeficientov. Z `zipWith` je rešitev še malo bolj elegantna:

    odvod :: Polinom -> Polinom
    odvod (Polinom []) = polinom []
    odvod (Polinom (_:koef)) = polinom $ zipWith (*) koef [1..]

### `integral`

Tudi tu je rešitev podobna (če delate z racionalnimi števili, seveda :)

    integral :: Polinom -> Polinom
    integral (Polinom koef) = polinom $ 0 : zipWith (/) koef [1..]

