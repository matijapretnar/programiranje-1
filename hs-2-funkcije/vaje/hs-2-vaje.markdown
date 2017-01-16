### Končni seznami

Lepo, da se zavedate konstruktorjev seznamov, a vseeno namesto `x : y : []` ali `x : [y]` pišite `[x, y]` - tako pri sestavljanju seznamov, kot pri razstavljanju v vzorcih.

### Komentarji posameznih nalog

### `range`

> Funkcija `range` naj dobi dva argumenta: `a`, `b`. Sestavi naj seznam števil od `a` do `b`.
> Funkcija naj bo definirana REKURZIVNO.
> Zgled:
> 
>     ghci> range 5 10
>     [5,6,7,8,9,10]

Ena izmed rešitev je

    range :: (Num t, Ord t) => t -> t -> [t]
    range a b
        | a > b = []
        | otherwise = a : range (a + 1) b

Elemente bi lahko dodajali tudi na drugem koncu z

    range a b
        | a > b = []
        | otherwise = range a (b - 1) ++ [b]

vendar zdaj že vemo, da to ni učinkovito.

Nekateri ste pisali tudi

    range a b
        | a == b = [a]
        | otherwise = a : range (a + 1) b

kar pa ni dobro, ker funkcija nima dobrega ustavitvenega pogoja, saj npr. `range 2 1` vrne `[2,3,4,5,...]`.

### `vstavi`

> Napišite funkcijo `vstavi`, ko dobi tri argumente: `x`, `l` in `i`. Funkcija naj v seznam `l` vstavi element `x` na `i`-to mesto. Funkcija naj bo definirana REKURZIVNO.
> Zgled:
> 
>     ghci> vstavi 7 [1,2,3,4,5] 2
>     [1,2,7,3,4,5]

Pri enostavni definiciji

    vstavi :: a -> [a] -> Int -> [a]
    vstavi x l 0 = x : l
    vstavi x (y:ys) i = y : vstavi x ys (i - 1)

vsi primeri niso pokriti. Na primer, kaj se zgodi, če je `i` negativen ali če je `i` pozitiven, `l` pa prazen. Glede na to, da navodila naloge tega ne določajo, lahko stvari pustimo kot so. Lahko pa bi dodali tudi primer

    vstavi _ [] _ = error "vstavi: napačen indeks"

če bi želeli javiti svojo napako ali pa

    vstavi x [] _ = [x]

če bi želeli vedno vrniti seznam.

Nekateri ste zgornjo definicijo pisali z robnimi pogoji:

    vstavi x l i
        | i == 0 = x : l
        | otherwise = head l : vstavi x (tail l) (i - 1)

Robne pogoje uporabljamo takrat, kadar vzorci niso dovolj. Če nas zanima le primerjava s konstanto, so vzorci dovolj. V tem primeru so vzorci celo boljši, saj lahko z njimi seznam `l` že razstavimo na glavo in rep. Nekateri ste namesto robnih pogojev uporabljali pogojne stavke

    vstavi x l i = if i == 0 then x : l else head l : vstavi x (tail l) (i - 1)

Tudi zaradi tega vas bom grdo gledal.


### `poparckaj`

> Sestavite funkcijo `poparckaj`, ki elemente seznama poparčka. Če je seznam
> lihe dolžine, zadnji element "zanemarimo". Funkcija naj bo definirana REKURZIVNO.
> Zgled:
> 
>     ghci> poparckaj ["Toni", "Majda", "Andrej", "Boris", "Petra"]
>     [("Toni","Majda"),("Andrej","Boris")]

Najelegantnejša rešitev je

    poparckaj :: [a] -> [(a, a)]
    poparckaj (x1:x2:xs) = (x1, x2) : poparckaj xs
    poparckaj _ = []

Kot vidite, ni treba, da definicije vedno začnemo s primerom za prazen seznam. Prvi primer bo polovil vse sezname dolžine vsaj dva, drugi pa prazen seznam in singleton.

Tudi tu ste nekateri z robnimi pogoji preverjali dolžino danega seznama, nato pa s funkcijami `head` in `tail` iz njega jemali ustrezne elemente. Bolje je uporabiti vzorce. Prav tako ste radi imeli sledeče:

    poparckaj xs
        | length xs >= 2 = (xs!!0, xs!!1) : poparckaj (drop 2 xs)
        | ...

Tudi tu so vzorci veliko elegantnejši in varnejši pristop. Kadarkoli uporabljate `length`, `null`, `!!`, pogojne stavke ali kaj takega, zelo verjetno še vedno razmišljate premalo funkcijsko.


### `jeNepadajoce`

> Sestavite funkcije `jeNepadajoce`, ki preveri, če so elementi seznama urejeni
> nepadajoče.
> Zgled:
> 
>     ghci> jeNepadajoce [-1,2,5,5,5,7,7]
>     True
>     ghci> jeNepadajoce [-1,2,5,3,5,7,7]
>     False

Kot zgoraj je rešitev z vzorci najenostavnejša:

    jeNepadajoce :: Ord a => [a] -> Bool
    jeNepadajoce (x1:x2:xs) = x1 <= x2 && jeNepadajoce (x2:xs)
    jeNepadajoce _ = True

Ta rešitev deluje z linearno časovno zahtevnostjo. Nekatere oddane rešitve so bile tudi oblike

    jeNepadajoce [] = True
    jeNepadajoce (x:xs) = x <= minimum xs && jeNepadajoce xs

kar pa deluje s kvadratično časovno zahtevnostjo. Če pričakujemo, da bo rep `xs` nepadajoč, bo minimum zagotovo prvi element, vendar Haskell tega sam ne more ugotoviti. Tako kot pri prejšnji nalogi tudi tu vidimo, da za razliko od predavanj v "resničnem" življenju ni vedno smiselno obravnavati le vzorcev `[]` in `x:xs` (v tem vrstnem redu).

Rešitev

    jeNepadajoce [] = True 
    jeNepadajoce (x:xs) = x < head xs && jeNepadajoce xs

pa ni v redu, ker ne deluje za sezname dolžine 1 (in posledično tudi za sezname dolžine več kot 1).

### `stirling2`

> Stirlingova števila druge vrste $S(n, k)$ štejejo razbitja $n$ elementne množice na $k$ nepraznih podmnožic. Za Strilingova števila druge vrste velja rekurzivna zveza:
> $S(n + 1, k) = k · S(n, k) + S(n, k - 1)$ za $k > 0$.
> Začetni pogoji so $S(n, 0) = S(0, n) = 0$ za $n > 0$ in $S(0, 0) = 1$.
> Zgled:
> 
>     ghci> stirling2 5 2
>     15

Tudi tu je rešitev z vzorci najbolj direktna (vendar kot bomo videli čez nekaj tednov na predavanjih, ne najbolj učinkovita):

    stirling2 :: Integer -> Integer -> Integer
    stirling2 0 0 = 1
    stirling2 _ 0 = 0
    stirling2 0 _ = 0
    stirling2 n k = k * stirling2 (n - 1) k + stirling2 (n - 1) (k - 1)

Svoje čase je Haskell podpiral tudi vzorce oblike `n + x`, zato ste lahko zadnji primer napisali kar kot

    stirling2 (n + 1) k = k * stirling2 n k + stirling2 n (k - 1)

vendar so to v Haskellu 2010 opustili.

Tudi rešitev 

    stirling2 n k
    | n == 0 && k == 0 = 1
    | n == 0 || k == 0 = 0
    | otherwise = k * stirling2 (n - 1) k + stirling2 (n - 1) (k - 1)

deluje pravilno, vendar je težje berljiva.

### `cantor`

> Sestavite funkcijo `cantor`, ki kot argument dobi nenegativno celo
> število $n$ in naj vrne niz dolžine $3^n$ z $n$-tim približkom Cantorjeve množice. 
> Zgled:
>
>     ghci> cantor 0
>     "*"
>     ghci> cantor 1
>     "* *"
>     ghci> cantor 2
>     "* *   * *"

To funkcijo ste že videli lani pri Uvodu v programiranje.

    cantor :: Int -> String
    cantor 0 = "*"
    cantor n = c' ++ replicate (3^(n - 1)) ' ' ++ c'
      where
        c' = cantor (n - 1)    

Za tip prvega argumenta bi lahko dali tudi `Integer`, vendar glede na hitrost naraščanja velikosti rezultata močno dvomim, da boste `cantor` klicali na številih, večjih od 9223372036854775807, kar je največji `Int`.

### `gcd'`

> Funkcija `gcd'` naj izračuna največji skupni delitelj dveh celih števil.
> Če sta oba argumenta 0, naj funkcija vrne 0.
> 
> Zgled:
>
>     ghci> gcd' 50 70 
>     10

Iskanje največjega skupnega delitelja je ena od stvari, pri katerih se definicije zelo rade nepotrebno zapletejo. Že Evklid je vedel, da je dovolj napisati le

    gcd' :: Integer -> Integer -> Integer
    gcd' m 0 = m
    gcd' m n = gcd n (m `mod` n)

(ali v drugem primeru, če želite, tudi `gcd n (mod m n)`.)
Karkoli več kot to je odveč. Če želite, da je rezultat pozitivno število, prvi primer spremenite v `gcd' m 0 = abs m`. Števil `m` in `n` ni treba urejati, prav tako ni treba gledati, ali je `n` enak nič.

Ena od rešitev je imela pravo idejo, vendar napačno izvedbo, saj je namesto `gcd n (mod m n)` pisalo `gcd n mod m n`. Kljub temu, da za klic funkcije oklepaji niso potrebni, jih vseeno potrebujemo, da pravilno sestavimo izraze. Tukaj namreč izraz pomeni isto kot `(((gcd n) mod) m) n` oziroma isto kot `(gcd n mod) m n`, torej najprej izračuna največji skupni delitelj števila `n` in funkcije `mod`, kar pa seveda ni smiselno.

Na [angleški Wikipediji](https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid.27s_algorithm) lahko najdete tudi sledeči algoritem

    gcd' m n
        | m == n = m
        | m > n = gcd' (m - n) n
        | m < n = gcd' m (n - m)

Njegova prednost je, da uporablja osnovnejše aritmetične operacije, vendar je precej bolj počasen. Razmislite, koliko korakov potrebuje prvi in koliko drugi algoritem, da izračuna `gcd' 10000 1`.

Pa še to, mogoče bi kdo rad zgornjo definicijo napisal kot

    gcd' m m = m
    gcd' m n = 
        | m > n = gcd' (m - n) n
        | m < n = gcd' m (n - m)

Takih definicij se v Haskellu ne da zapisati. Argumenta je treba najprej shraniti v dve različni spremenljivki, nato pa pogledati, ali sta njuni vrednosti enaki.

Sicer pa vas globoko iz srca prosim, da ne uporabljate algoritma opisanega na [slovenski Wikipediji](https://sl.wikipedia.org/wiki/Največji_skupni_delitelj):

    gcd' m n = max [d | d <- [1..m], m `mod` d == 0, n `mod` d == 0]


### `permutacije`

> Sestavite funkcijo `permutacije`, ki kot argument dobi seznam in vrne seznam
> vseh permutacij tega seznama.
> Zgled:
> 
>     ghci> permutacije [1,2,3]
>     [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Vse permutacije seznama naredimo tako, da naredimo vse permutacije repa, nato pa glavo vrinemo na vsa možna mesta:

    permutacije :: [a] -> [[a]]
    permutacije [] = [[]]
    permutacije (x:xs) = [perm | xs' <- permutacije xs, perm <- vrini x xs']
      where
        vrini x [] = [[x]]
        vrini x (y:ys) = (x:y:ys) : map (y:) (vrini x ys)

Pri tem `map (y:) (vrini x ys)` pomeni, da `x` na vse načine vrinemo v rep `ys`, nato pa vsaki od možnih kombinacij na začetek dodamo še `y`.

Druga možnost je, da za vsako možno glavo dodamo vse možne permutacije repa. Če je dani seznam urejen, so vrnjene permutacije celo v leksikografskem vrstnem redu.

    permutacije [] = [[]]
    permutacije l = [x:xs' | (x, xs) <- razdeli l, xs' <- permutacije xs]
      where
        razdeli [] = []
        razdeli (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (razdeli xs)

Ta rešitev ima slabost, da se za seznam dolžine $n$ rekurzivno pokliče $n!$-krat. Prva različica se pokliče le $n$-krat. Seveda imata obe različici še vedno časovno zahtevnost $O(n!)$, saj morata obe vrniti seznam dolžine $n!$, a s `:set +s` lahko preizkusite, da prva deluje nekajkrat hitreje.

Tretja možnost pa je, da uvozite modul `Data.List` in pišete

    permutacije = permutations

ki deluje blazno hitreje od vseh zgoraj napisanih.

### `hanoi`

> Funkcija `hanoi` naj kot argument dobi tri cela števila: `n`, `a`, `b`, kjer je `1 <= a, b <= 3` in `a /= b`. Vrne naj seznam parov, ki predstavljajo premike, s katerimi prestavimo `n` diskov
> spravimo s palice `a` na palico `b`.
> Zgled: 
>
>     ghci> hanoi 3 1 3
>     [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
>     ghci> hanoi 4 1 3
>     [(1,2),(1,3),(2,3),(1,2),(3,1),(3,2),(1,2),(1,3),(2,3),(2,1),(3,1),(2,3),(1,2),(1,3),(2,3)]

Ideja je sledeča: $n$ plošč premaknemo s palice A na palico C tako, da najprej $n - 1$ plošč premaknemo z A na B. Pri tem na največjo ploščo gledamo kot na del podlage, tako da v resnici rekurzivno rešujemo problem za manjše število plošč. Po tem zadnjo ploščo damo z A na C, nato pa spet $n - 1$ plošč rekurzivno premaknemo z B na C. Ko moramo premakniti $0$ plošč, nam ni treba storiti ničesar.

    hanoi :: Int -> Int -> Int -> [(Int,Int)]
    hanoi 0 _ _ = []
    hanoi n a c = hanoi (n - 1) a b ++ [(a, c)] ++ hanoi (n - 1) b c
      where b = 6 - a - c

Tako kot pri `cantor` je tudi tu dovolj, če argumente omejite na `Int`.
