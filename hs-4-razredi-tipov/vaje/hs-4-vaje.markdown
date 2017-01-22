## `instance Num OdPrejsnjihVaj`

### `instance Num Kompleksno`

Tu so vam stvari načeloma šle (z izjemo par računskih spodrsljajev, ki niso moja odgovornost, vas bom pa zatožil profesorju za Analizo):

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

## Razredi tipov


    -- Algebra
    -- =======
    
    
    -- Definirajmo razred Polgrupa kot:
    
    class  Polgrupa a  where
        (***) :: a -> a -> a
    
    -- Definirajte še razreda PolgrupaZEnoto (s posebno vrednostjo "enota") in
    -- Grupa (s posebno vrednostjo "inv"), ter pokažite, da vsem trem pripadajo:
    -- * cela števila za seštevanje
    -- * kartezični produkt (po komponentah)
    -- * če imate preveč časa, vprašajte asistenta za ideje
    
    class  Polgrupa a => PolgrupaZEnoto a  where
        enota :: a
    
    class  PolgrupaZEnoto a => Grupa a  where
        inv :: a -> a
    
    
    instance  Polgrupa Integer  where
        (***) = (+)
    
    instance  PolgrupaZEnoto Integer  where
        enota = 0
    
    instance  Grupa Integer  where
        inv = negate
    
    
    instance  (Polgrupa a, Polgrupa b) => Polgrupa (a, b)  where
        (x1, y1) *** (x2, y2) = (x1 *** x2, y1 *** y2)
    
    instance  (PolgrupaZEnoto a, PolgrupaZEnoto b) => PolgrupaZEnoto (a, b)  where
        enota = (enota, enota)
    
    instance  (Grupa a, Grupa b) => Grupa (a, b)  where
        inv (x, y) = (inv x, inv y)
    
 
Definicije so bile zelo podobne tistim z vektorskimi prostori, ki smo jih naredili na predavanjih. Pozorni bodite na to, da vam signatur ni treba ponavljati. Torej, kljub temu, da ima `PolgrupaZEnoto` tudi operacijo `(***)`, vam ni treba pisati

    class  Polgrupa a => PolgrupaZEnoto a  where
        (***) :: a -> a -> a
        enota :: a

saj je ravno to zajeto v `Polgrupa a =>`. Poleg tega vam Haskell tega ne pusti, ker ima `(***)` že drug pomen. Prav tako ne pišite enačb, ki naj bi veljale:

    class  Polgrupa a => PolgrupaZEnoto a  where
        enota :: a
        x *** enota = x
        enota *** x = x

saj se `enota` v `x *** enota = x` obnaša kot običajna spremenljivka, zato s tem definirate `(***)` kot konstantno funkcijo v vseh članih razreda, ki nimajo svoje definicije za `(***)`. Zadnja vrstica nima vpliva, ker že prejšnja vrstica polovi vse primere.


## Porazdelitve

    -- Porazdelitve
    -- ============
    
    data Porazdelitev a = Porazdelitev [(a, Rational)] deriving Show
    
    kovanec = Porazdelitev [("cifra", 1/2), ("grb", 1/2)]
    kocka = Porazdelitev [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

### `jePorazdelitev`

    -- Funkcija naj preveri, če je porazdelitev zares porazdelitev,
    -- tj. če se verjetnosti seštejejo v 1.
    
    jePorazdelitev :: Porazdelitev a -> Bool
    jePorazdelitev (Porazdelitev d) = sum (map snd d) == 1

Nekateri ste uporabili izpeljane sezname ali pa namesto `snd` pisali `\(_, p) -> p`. Tudi to je v redu. Manj všeč pa mi je, da vas je kar precej pisalo tudi `if sum (map snd d) == 1 then True else False` ali pa

    jePorazdelitev (Porazdelitev d) =
    | sum (map snd d) == 1 = True
    | otherwise = False

Oboje načeloma deluje pravilno, vendar ni higienično. Take stvari se začnejo nabirati in programi hitro postanejo nepregledni.
    
### `urediPorazdelitev`

    -- Funkcija urediPorazdelitev naj "porihta" porazdelitev,
    -- tj. dogodke naj uredi in združi skupaj enake dogodke.
    
    urediPorazdelitev :: Ord a => Porazdelitev a -> Porazdelitev a
    urediPorazdelitev (Porazdelitev d) =
        Porazdelitev $ zdruzi $ sortBy (comparing fst) $ d
        where
        zdruzi ((x1,p1):(x2,p2):xs)
            | x1 == x2 = zdruzi ((x1, p1 + p2):xs)
            | otherwise = (x1,p1) : zdruzi ((x2, p2) : xs)
        zdruzi xs = xs

Kot vidite, lahko seznam uredimo glede na neko lastnost, v našem primeru prvo komponento, če uporabimo `sortBy`, ki ureja glede na primerjalno funkcijo, in `comparing`, ki ustvari primerjalno funkcijo glede na vrednosti slik. Po asistentovem predlogu (ki ga je dobil od mene, preden sem izvedel za `comparing`), ste isto lahko dosegli tudi z ``compare `on` fst``.

Pri združevanju z izjemo vzorcev večjih težav ni bilo. Jih je bilo pa zato tam toliko več :) Ena je, da ne ločite dobro med tem, kaj sodi v vzorec in kaj v robni pogoj. Pravilo je enostavno: v vzorcu spremenljivkam prirejamo vrednosti, v robnem pogoju pa obstoječe izraze primerjamo med seboj. Na primer, če želite sešteti komponenti para, boste pisali

    f (x, y) = x + y

in ne

    f par | (x, y) == par = x + y

saj v pogoju para `par` ne morete primerjati z `(x, y)`, ker `x` in `y` še nista definirana. Obratno, če želite pare z enakimi komponentami slikati v `1`, boste pisali

    g (x, y) | x == y = 1

ne pa

    g (x, x) = 1

saj v tem primeru spremenljivki `x` prirejamo obe komponenti para.

V konkretnem primeru funkcije `zdruzi` ste želeli posebej obravnavati sezname z vsaj dvema elementoma in ste pisali nekaj v stilu

    zdruzi xs'
        | xs' == ((x1,p1):(x2,p2):xs) and x1 == x2 = zdruzi ((x1, p1 + p2):xs)

kar seveda ni prav, ker Haskell ne ve, s kakšnimi `x1`, `x2`, … želite primerjati `xs'`. Pisati morate:

    zdruzi ((x1,p1):(x2,p2):xs)
        | x1 == x2 = zdruzi ((x1, p1 + p2):xs)

in podobno naprej. Potem ste radi zgornji primer pisali tudi kot

    zdruzi xs
        | fst $ head xs == fst $ head $ tail xs = zdruzi ((fst $ head xs, ...))

(preostanek bom iz prijaznosti izpustil). Povem še enkrat tako kot zadnjič: če uporabljate `head`, `tail` in podobne, razmislite, ali ne bi bili vzorci primernejša rešitev.

V pogoje ste radi tlačili tudi `sort`, na primer

    zdruzi xs
        | fst $ head (sort xs) == fst $ head $ tail (sort xs) = ...

kar pa je seveda popolnoma neučinkovito. Funkcije vedno razstavljajte na manjše smiselne dele. V tem primeru torej seznam najprej uredite, nato pa z ločeno funkcijo združite enake elemente.

Še zadnja stvar: ugotovili ste, da prazne sezname in sezname z enim elementom pustite pri miru. V tem primeru ne morete pisati

    zdruzi [] = []
    zdruzi [_] = [_]

saj lahko `_` uporabljate le v vzorcih, kadar vas vrednost ne zanima. Na desni tega ne morete storiti, saj morate povedati, kakšen element naj bo v seznamu.

### `najverjetnejsi`

    -- Funkcija najverjetnejsi naj vzame porazdelitev in vrne
    -- najverjetnejši izid (če jih je več, tistega, ki je najbolj desno).
    
    najverjetnejsi :: Porazdelitev a -> a
    najverjetnejsi (Porazdelitev d) = foldr1 maksi d
      where
        maksi (x1, p1) (x2, p2) = if p1 > p2 then (p1, x1) else (p2, x2)

Zgornjo definicijo bi lahko definirali tudi rekurzivno in brez `foldr1`, lahko pa bi si pomagali tudi z `maximumBy`. Če smo malo površni, lahko pišemo

    najverjetnejsi (Porazdelitev d) =
        fst $ maximumBy (comparing snd) d in

vendar pri tem nismo čisto prepričani, ali bo v primeru več elementov z enakimi verjetnostmi funkcija vrnila najbolj desnega. Da se prepričamo, lahko uredimo najprej po verjetnosti, nato pa še po indeksu z:

    najverjetnejsi (Porazdelitev d) =
        let ((x, _), _) = maximumBy (comparing (\((_, p), n) -> (p, n))) (zip d [0..]) in
        x

Nekateri ste seznam verjetnosti poprej še uredili, kar za računanje maksimuma nepotrebno in časovno zahtevnost poveča z $O(n)$ na $O(n \log n)$. Seveda privzemamo, da je porazdelitev urejena in se vsak element pojavi le enkrat.


### `enakomerna`
   
    -- Funkcija enakomerna naj vzame seznam in vrne enakomerno
    -- porazdelitev, kjer so dogodki elementi tega seznama.
    -- Namig: Int lahko pretvorite v Rational z "fromIntegral"
    
    enakomerna :: [a] -> Porazdelitev a
    enakomerna xs =
        Porazdelitev [(x, p) | x <- xs]
        where
        p = 1 / (fromIntegral $ length xs)

Tale vam je načeloma šla, tako da nimam posebnih pripomb.

### `utezenaVsota`

    -- Funkcija utezenaVsota na vzame neko decimalno število p med 0 in 1 in vrne
    -- uteženo vsoto dveh porazdelitev, kjer je prva utežena s p, druga pa z
    -- (1 - p). Predpostavimo, da sta obe porazdelitvi porihtani in ju lahko
    -- združimo z zlivanjem.
    
    pomnozi :: Rational -> [(a, Rational)] -> [(a, Rational)]
    pomnozi p d = [(x, p * q) | (x, q) <- d]
    
    utezenaVsota :: Ord a => Rational -> Porazdelitev a -> Porazdelitev a -> Porazdelitev a
    utezenaVsota p (Porazdelitev d1) (Porazdelitev d2) =
        urediPorazdelitev $ Porazdelitev $ pomnozi p d1 ++ pomnozi (1 - p) d2

Za potrebe te definicije in definicije funkcije `zdruzi` sem si definiral pomožno funkcijo `pomnozi`, ki pa sem jo napisal kar na tipu `[(a, Rational)]`,
saj ne ohranja porazdelitev (vsota ni več 1), zato je bolje, da to odraža tudi njen tip.

### `upanje`

    -- Funkcija upanje naj dobi slučajno spremenljivko, kot funkcijo iz a v Rational
    -- in naj vrne matematično upanje.
    
    upanje :: (a -> Rational) -> Porazdelitev a -> Rational
    upanje f (Porazdelitev d) = sum [p * f x | (x, p) <- d]

Tudi ta naloga vam je šla brez večjih težav.

### `zdruzi`

    -- Funkcija zdruzi naj dobi porazdelitve porazdelitev in jih "združi".
    
    zdruzi :: Ord a => Porazdelitev (Porazdelitev a) -> Porazdelitev a
    zdruzi (Porazdelitev ds) =
        urediPorazdelitev $ Porazdelitev $ foldr (\(Porazdelitev d1, p) d2 -> pomnozi p d1 ++ d2) [] ds

Tale je malo bolj zapletena različica funkcije `utezenaVsota`. Načeloma bi lahko z njeno pomočjo definirali tudi

    utezenaVsota p d1 d2 = zdruzi $ Porazdelitev $ [(d1, p), (d2, 1 - p)]

Prednost (no, kakor za koga) Haskella je ta, da lahko funkcije definirate v poljubnem vrstnem redu, torej `utezenaVsota` definirate s pomočjo `zdruzi`, ki jo definirate šele kasneje.

### `Functor Porazdelitev`

    -- Pokažite, da konstruktor tipov Porazdelitev pripada razredu Functor.
    
    instance  Functor Porazdelitev  where
        fmap f (Porazdelitev d) = Porazdelitev [(f x, p) | (x, p) <- d]

Torej, če imamo `f :: a -> b` in porazdelitev na `a`, želimo dobiti porazdelitev na `b`. Očitno moramo le vsak element slikati z `f`. Še bolje bi bilo, če bi porazdelitev potem tudi uredili s funkcijo `urediPorazdelitev`. Na primer, kaj se zgodi, če porazdelitev `kocka` slikamo s funkcijo `odd`?

Toda tega žal ne moremo storiti, saj `urediPorazdelitev` zahteva `Ord a`, tega pogoja pa nam signatura razreda `Functor` ne dovoli zapisati. Če kdo od vas s tem ni zadovoljen, si lahko ogleda modul [`Control.Functor.Constrained`](https://hackage.haskell.org/package/constrained-categories-0.2.1.1/docs/Control-Functor-Constrained.html).
