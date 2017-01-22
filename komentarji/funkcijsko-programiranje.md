### `prvaBeseda`

> Sestavite funkcijo `prvaBeseda`, ki kot argument dobi nek niz in vrne prvo besedo v nizu. Besede v nizu so med seboj ločene s presledki. Nalogo rešite z uporabo funkcije `takeWhile`. Funkcija naj deluje tudi, če se niz začne s presledkom.
> Zgled:
> 
>     ghci> prvaBeseda "Banane so dobre."
>     "Banane"
>     ghci> prvaBeseda "  Banane so dobre."
>     "Banane"

Kot svetuje naloga, s `takeWhile` poberemo vse črke do prvega presledka. Pred tem pa z `dropWhile` pobrišemo vse začetne presledke:

    prvaBeseda :: String -> String
    prvaBeseda niz = takeWhile (/= ' ') (dropWhile (== ' ') niz)

ali pa še bolje kot

    prvaBeseda niz = takeWhile (/= ' ') $ dropWhile (== ' ') $ niz

ali pa še bolje kot

    prvaBeseda = takeWhile (/= ' ') . dropWhile (== ' ')

ali pa kar

    prvaBeseda = head . words

Mogoče se vam zdi, da je ta rešitev neučinkovita, saj naredi seznam vseh besed, mi pa potrebujemo le prvo (recimo, da želimo najti le prvo besedo v zelo zelo dolgem romanu). Toda Haskell je len, zato bo funkcija `words` naredila le prvi element seznama, ki ga bo nato predala funkciji `head`, ta pa ga bo vrnila in končala izvajanje.

### `seznamBesed`

> Z uporabo funkcij `takeWhile` in `dropWhile` sestavite funkcijo `seznamBesed`, ki dobi niz in vrne seznam besed v tem nizu.
> Zgled:
> 
>     ghci> seznamBesed "Banana je dobra."
>     ["Banana","je","dobra."]

Napisali bi lahko

    seznamBesed :: String -> [String]
    seznamBesed = razdeli . dropWhile (== ' ')
      where
        razdeli "" = []
        razdeli niz = takeWhile (/= ' ') niz : (seznamBesed $ dropWhile (/= ' ') niz)

Funkcija je precej robustna, saj deluje tudi takrat, ko je med besedami več kot en presledek. Če bi želeli, da so med besedami lahko tudi drugi prazni znaki (znak za novo vrstico ali tabulator, nedeljivi presledek, …), bi lahko uvozili modul `Data.Char` in pisali

    seznamBesed = razdeli . dropWhile isSpace
      where
        razdeli "" = []
        razdeli niz = takeWhile (not . isSpace) niz : (seznamBesed $ dropWhile (not . isSpace) niz)

ali pa seveda kar

    seznamBesed = words


### `eksponentna`

> Sestavite funkcijo `eksponentna`, ki dobi številska argumenta `a` in `x` ter vrne `x^a`. Nato definirajte funkcijo `kvadriraj`, ki kot argument dobi število `x` in vrne `x^2`. Pri njeni definiciji uporabite funkcijo eksponentna.
> Zgled:
> 
>     ghci> eksponentna 10 2
>     1024
>     ghci> kvadriraj 7
>     49

Tu ni veliko izbire:

    eksponentna :: Integer -> Integer -> Integer
    eksponentna a x = x^a
    
    kvadriraj :: Integer -> Integer
    kvadriraj = eksponentna 2

edino lahko bi se malo prese\*avali in napisali

    eksponentna = flip (^)


Nektari ste privzeli, da morate eksponentno funkcijo napisati sami in ste definirali:

    eksponentna a x
        | a == 0 = 1
        | a == 1 = x
        | otherwise = x * eksponentna (a - 1) x

Lepše bi bilo (seveda tudi pri vseh drugih stranskih pogojih oblike `x == []`, `x == ""`, …)

    eksponentna 0 x = 1
    eksponentna 1 x = x
    eksponentna a x = x * eksponentna (a - 1) x

pa tudi drugi primer lahko izpustite. Ko bomo delali _deli in vladaj_ me spomnite, da pogledamo, kako eksponentno funkcijo implementiramo bolj učinkovito in sicer v času $O(\log a)$ (zgornja rešitev namreč deluje v linearnem času).


### `linearna`

> Napišite funkcijo linearna, ki kot argumenta dobi števila a in b ter vrne linearno funkcijo x -> a x + b. Nato
> definirajte funkcijo f, ki slika x -> 3 x - 2.
> 
> Zgled:
> ghci> linearna 1 2 0.5
> 2.5
> ghci> f 2
> 4

Tu bi lahko pisali

    linearna :: Num a => a -> a -> a -> a
    linearna a b x = a * x + b

    f :: Num a => a -> a
    f = linearna 3 (-2)

edino pri prvi bi bilo mogoče lepše pisati

    linearna a b = \x -> a * x + b

Kdaj pišemo eno in kdaj drugo obliko je stvar okusa. Načeloma pri definiciji napišemo toliko argumentov, kot jih bomo običajno sprejeli. Na primer, `(.)` je definiran kot

    (.) :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)

in ne kot `(.) f g x = f (g x)` ali kot `(.) = \f -> \g -> \x -> f (g x)`, saj ga ponavadi uporabljamo na prvi način (torej med dvema funkcijama). Ampak, kot sem rekel, stvar okusa.

### `norma`

> Napišite funkcijo `norma`, ki kot argument dobi število $p$, $1 \le p$, in izračuna $p$-to normo vektorja (ki je podan s seznamom).
> Zgled:
> 
>     ghci> norma 1 [-1,0,3,2]
>     6.0
>     ghci> let n2 = norma 2
>     ghci> n2 [-1,0,3,2]
>     3.7416573867739413

Verjetno najkrajši način je

    norma :: Floating a => a -> [a] -> a
    norma p v = sum [abs x ** p | x <- v] ** (1 / p)

Asistent Bašić je predlagal, da bi imeli še $\infty$-normo. Ker števila s plavajočo vejico podpirajo neskončno vrednost, ki jo dobite, če izračunate `1 / 0`, lahko pišete tudi

    norma p v
        | p == 1 / 0 = maximum [abs x | x <- v]
        | otherwise = sum [abs x ** p | x <- v] ** (1 / p)

### `integral f a b d`

> Sestavite funkcijo `integral f a b d`, ki numerično izračuna integral funkcije $f$ (realna funkcija ene spremenljivke)
> v mejah od $a$ do $b$. Integral izračunajte po Riemannovo in sicer uporabite delitev
> $$
>   \{
>     t_0 = a, t_1 = a + d, t_2 = a + 2d, t_3 = a + 3d, …, t_{n−1} = a + (n−1)d, t_n=b
>   \},
> $$
> kjer je $a + (n − 1) d < b \leq a + n d$, in izračunajte
> 
> $$\sum_{i=0}^{n−1} f((t_i + t_{i+1}) / 2) (t_{i+1} − t_i).$$
> 
> Zgled:
> 
>     ghci> integral (\x -> x^2 + 3*x + 4) 1 3 0.01
>     28.666650000000374
> 
> Opomba: Funkcija mora delovati tudi, če je $a > b$. V tem primeru zamenjajte meji in upoštevajte, da se predznak integrala spremeni.

Verjetno najbolj enostavna je sledeča rešitev (zahvala gre vašemu asistentu - moja rešitev je bila bolj nerodna in je sploh ne bom kazal):

    integral :: RealFrac a => (a -> a) -> a -> a -> a -> a
    integral f a b d
        | a > b = -integral f b a d
        | a + d > b = f ((a + b) / 2) * (b - a)
        | otherwise = f (a + (d / 2)) * d + integral f (a + d) b d


### `uporabiNkrat`

> Sestavite funkcijo `uporabiNkrat f x n`, ki kot argumente dobi funkcijo `f`, število `x` in naravno število `n` ter vrne število,
> ki ga dobimo, če na `x` funkcijo `f` uporabimo `n`-krat. Zgled:
> 
>     collatz x
>         | even x = x `div` 2
>         | otherwise = 3*x + 1
>    
>     ghci> uporabiNkrat collatz 13 7
>     4

Osnovna pristopa sta dva:

    uporabiNkrat :: (a -> a) -> a -> Integer -> a
    uporabiNkrat f x 0 = x
    uporabiNkrat f x n = uporabiNkrat f (f x) (n - 1)

ali pa

    uporabiNkrat f x 0 = x
    uporabiNkrat f x n = f $ uporabiNkrat f x (n - 1)

Pri tem je načeloma prva različica boljša zaradi tega, ker je _repno rekurzivna_ (_tail recursive_). V Haskellu to niti ni preveč pomembno, ker se zaradi lenosti stvari dostikrat izvajajo čisto drugače, kot bi pričakovali, ampak ideja je sledeča. Prva različica deluje kot

      uporabiNkrat succ 0 100
    = uporabiNkrat succ 1 99
    = uporabiNkrat succ 2 98
    = ...
    = uporabiNkrat succ 99 1
    = uporabiNkrat succ 100 0
    = 100

druga pa kot

      uporabiNkrat succ 0 100
    = succ $ uporabiNkrat succ 0 99
    = succ $ succ $ uporabiNkrat succ 0 98
    = ...
    = succ $ succ $ ... $ succ $ uporabiNkrat succ 0 1
    = succ $ succ $ ... $ succ $ succ $ uporabiNkrat succ 0 0
    = succ $ succ $ ... $ succ $ succ $ 0
    = succ $ succ $ ... $ succ $ 1
    = ...
    = succ $ 99
    = 100

Torej si mora računalnik zapomniti vse funkcije, ki jih bo moral izvesti po tem, ko bo `uporabiNkrat` izvedel svoj zadnji klic. Pravimo, da jih mora naložiti na _sklad klicev_ (_call stack_), ki pa je ponavadi omejen. Več o tem pa kdaj drugič in kje drugje (po želji pa seveda tudi na tem forumu).

Pri eni izmed teh rešitev je bilo tudi vprašanje, kako naj funkciji določimo tip. Pristopa sta dva: (1) dobro razmislimo, kakšen bi bil pravilen tip, torej kaj naj bi kakšen argument pomenil – ta nasvet ne pomaga preveč, vem, zato imam še (2) ko funkcijo definiramo, v konzoli uporabimo `:t imeFunkcije`, da vidimo, kakšen tip je izračunal Haskell, nato pa ga po potrebi popravimo.

### `uporabiVse`

> Sestavite funkcijo `uporabiVse`, ki kot argumenta dobi seznama funkcij `sez` in število `x`. Funkcija naj sestavi in vrne seznam števil, ki ga
> dobi tako, da na `x` po vrsti uporabi vsako funkcijo s seznama `sez`. Zgled:

>     ghci> let f x = x^2 - 3*x + 4
>     ghci> let g x = x**x
>     ghci> uporabiVse [sin, cos, f, g, abs] (pi / 2)
>     [1.0,6.123233995736766e-17,1.7550121198876498,2.032658322210728,1.5707963267948966]

Ker je `($)` infiksna operacija, ki levi argument uporabi na desnem, lahko pišemo

    uporabiVse :: [a -> b] -> a -> [b]
    uporabiVse sez x = map ($ x) sez

čeprav je verjetno bolj berljivo:

    uporabiVse sez x = [f x | f <- sez]

Všeč mi je bila tudi rešitev

    uporabiVse sez x = zipWith ($) sez [x,x..]

čeprav bi bilo zadnji seznam bolje pisati kot `repeat x`, saj v tem primeru ni treba zahtevati, da tip spremenljivke `x` pripada razredu `Enum`.

Če ste bolj po Pythonovsko pisali

    uporabiVse sez x = [(sez !! i) x | i <- [0..(length sez - 1)]]

se zavedajte, da je časovna zahtevnost te rešitve kvadratična, saj gre `(!!)` vsakič od začetka čez seznam, da poišče ustrezen element.


### `zdruzi`

> Sestavite funkcijo `zdruzi`, ki dobi seznam nizov in vrne niz, ki ga dobi tako, da stakne nize iz danega seznama, med njih pa postavi presledke. Pri reševanju uporabite `foldr` ali `foldl`.
> Zgled:
> 
>     ghci> zdruzi ["Banana","je","dobra."]
>     "Banana je dobra."

Seveda je to kar

    zdruzi :: [String] -> String
    zdruzi = unwords

ampak če želite uporabiti `foldr`, morate pisati

    zdruzi = init $ foldr (\x acc -> x ++ " " ++ acc) ""

ali še bolje

    zdruzi = foldr1 (\x acc -> x ++ " " ++ acc)

Za `foldl1` pa bi pisali

    zdruzi = foldl1 (\acc x -> acc ++ " " ++ x)

Razlika je torej, ali `x` k akumulatorju `acc` dodamo na levi ali na desni.

### `poparckaj`

> Sestavite funkcijo `poparckaj`, ki elemente seznama poparčka. Če je seznam
> lihe dolžine, zadnji element "zanemarimo". Uporabite funkcijo `foldl`.
> Zgled:
> 
>     ghci> poparckaj ["Toni", "Majda", "Andrej", "Boris", "Petra"]
>     [("Toni","Majda"),("Andrej","Boris")]

Pri zlaganju v akumulatorju vedno nosimo vse podatke, ki jih trenutno rabimo.Tokrat v akumulatorju ne bomo vozili le seznama parov, temveč tudi morebitnega člana novega para (torej `Maybe a`), ki ga bomo na koncu s `fst` vrgli stran. Ker `foldl` pare nabiramo od leve proti desni, torej bodo najbolj desni na vrhu vrnjenega seznama, bomo zadevo na koncu še obrnili z `reverse`, pa bo.

    poparckaj :: [a] -> [(a, a)]
    poparckaj = reverse . fst . foldl dodaj ([], Nothing)
      where
        dodaj (pari, Just x) y = ((x, y) : pari, Nothing)
        dodaj (pari, Nothing) y = (pari, Just y)

Nekateri ste imeli tudi rešitev sledeče vrste

    poparckaj = reverse . fst3 . foldl dodaj ([], False, "")
      where
        dodaj (pari, True, x) y = ((x, y) : pari, False, "")
        dodaj (pari, False, "") y = (pari, True, y)
        fst3 (x, _, _) = x

Torej, morebitnega člana novega para ste predstavili z imenom ter logično vrednostjo, ki je povedala, ali član obstaja ali ne. Če je bila vrednost `False` ste za ime dali fiktivno vrednost, na primer `""` (nekdo je uporabil tudi `"neki neprimernega"` :). Kot vidite, gre za iste podatke kot zgoraj, le da tip `Maybe String` bolje zajame dejanske možne vrednosti, s čimer obstaja manjša možnost za napake. V drugi varianti morate na primer sami poskrbeti, da so vsi trije fiktivni nizi enaki `""`, med tem ko bo Haskell namesto vas poskrbel, da bo vrednost vedno `Nothing` ali `Just ime`. Poleg tega vam ta način preprečuje, da bi imeli splošni tip `poparckaj :: [a] -> [(a, a)]`, saj si ne morete izmisliti splošne vrednosti tipa `a`, zato ima `poparckaj` tip `[String] -> [(String, String)]`.

### `jeNepadajoce`

> Sestavite funkcije `jeNepadajoce`, ki preveri, če so elementi seznama urejeni
> nepadajoče. Uporabite funkcijo `foldr` ali `foldl`.
> Zgled:
> 
>     ghci> jeNepadajoce [-1,2,5,5,5,7,7]
>     True
>     ghci> jeNepadajoce [-1,2,5,3,5,7,7]
>     False

Obstajata dve možnosti: seznam je do sedaj nepadajoč ali pa smo prišli do napake. V prvem primeru nas zanima preostanek seznama, torej potrebujemo zadnji videni element, v drugem pa nič. Tako je dovolj, če vozimo `Maybe a`. Na koncu pogledamo, ali smo dobili `Nothing` (torej seznam ni nepadajoč) ali ne (v tem primeru seznam je nepadajoč).

    jeNepadajoce :: Ord a => [a] -> Bool
    jeNepadajoce [] = True
    jeNepadajoce (x:xs) = foldl preveri (Just x) xs /= Nothing
      where
        preveri Nothing _ = Nothing
        preveri (Just x) y
            | x <= y = Just y
            | otherwise = Nothing

Druga rešitev pa je, da vozimo logično vrednost in zadnji element:

    jeNepadajoce [] = True
    jeNepadajoce (x:xs) = fst $ foldl preveri (Just x) xs
      where
        preveri (nepadajoc, x) y = nepadajoc && x <= y

V nasprotju s prejšnjim primerom ta rešitev nima pomankljivosti.
Nekateri ste pisali različico spodnjega:

    jeNepadajoce [] = True
    jeNepadajoce (x:xs) = fst $ foldl preveri (Just x) xs
      where
        preveri acc y = fst acc && snd acc <= y

Vzorci so vaši prijatelji – uporabite jih.

Možna rešitev je tudi, da seznam `[x1, x2, x3, ...]` pretvorite v seznam zaporednih parov `[(x1, x2), (x2, x3), ...]`, nato pa preverite, da je vsak od njih nepadajoč. Elegantno se to napiše kot:

    jeNepadajoce [] = True
    jeNepadajoce xs@(_:xs') = all $ zipWith (<=) xs xs'

torej, če je seznam prazen, je nepadajoč, sicer pa postopno združujemo seznam in njegov rep. Parov nam ni potrebno delati, saj zaporedne elemente združimo kar s funkcijo `(<=)`. Iz tega dobimo seznam tipa `[Bool]` in z `all` preverimo, ali so vse vrednosti v njem enake `True`. Ker je Haskell len, se ti seznami gradijo po potrebi, zato bo funkcija že ob prvem neustreznem paru vrnila `False`. To lahko preverite s klicem `jeNepadajoce (2:[1..])` ki očitno vrne `False` še preden pregleda ves neskončen seznam.


### `disemvoweling`

> Sestavite funkcijo `disemvoweling`, ki dobi niz in vrne 'disemvoweling' tega niza.
> Le tega dobimo tako, da vse samoglasnike prestavimo na konec niza. Uporabite funkcijo
> `filter`.
> Zgled:
>  
>     ghci> disemvoweling "Banana je dobra."
>     "Bnn j dbr.aaaeoa"

Če želite uporabiti `filter`, kot pravi nasvet, lahko napišete:

    disemvoweling :: String -> String
    disemvoweling niz =
        filter (not . samoglasnik) niz ++ filter samoglasnik niz
          where
            samoglasnik x = x `elem` "aeiouAEIOU"

ali pa seveda, kot pravi hekerji uvozite funkcijo `partition` iz modula `Data.List` in napišete:

    disemvoweling = uncurry (++) . partition (`elem` "aeiouAEIOU")

Nekateri ste si definirali pomožno funkcijo

    niElem x niz = if x `elem` niz then False else True

čeprav bi bilo v tem primeru bolje pisati

    niElem x niz = not (x `elem` niz)

ali pa kar `notElem`, saj ta funkcija že obstaja. Cenim tudi trud tistih, ki ste namesto `notElem "aeiouAEIOU"` pisali `elem "QWRTZPŠĐSDFGHJKLČĆŽYXCVBNMqwrtzpšđsdfghjklčćžyxcvbnm.,!? "`.
