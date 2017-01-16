Objavljam komentarje 1. vaj iz Haskella. Komentarje sem skopiral od lani in dopolnil. Na splošno sem z letošnjimi oddajami kar zadovoljen in možno je, da se kakšna od spodaj omenjenih napak ni pojavila v nobeni oddaji. Vseeno sem komentarje obdržal, ker se učimo tudi iz tujih napak. Če vas karkoli zanima ali vam karkoli ni jasno, bom zelo vesel, če to vprašate na forumu. Na splošno bi si želel, da čimveč sprašujete na forumu, saj smo na predavanjih omejeni s časom, zato vsega ne uspem razložiti tako, kot bi bilo prav.

Najprej nekaj splošnih komentarjev.

### Presledki

Nekateri še vedno zelo neradi vstavljate presledke v kodo. Namig: največja tipka na tipkovnici je velika s tem namenom, da jo je čimbolj enostavno pritisniti.

### Signature

Veliko vas pred definicije funkcij ni pisalo signatur. Signature so koristne iz treh razlogov:

1. Služijo kot dokumentacija, saj tipi v kombinaciji z ustreznim imenom že precej dobro opišejo, kaj naj bi funkcija počela.

2. Preprečijo napake, pri katerih napisani program ima tip, vendar ne želenega. Npr. če napišemo

       podvoji xs = [[x, x] | x <- xs]

   smo res napisali veljaven program, ki ima tip, vendar ne počne tega, kar zahteva naloga.

3. Poenostavijo obvestila o napakah. Haskell zna biti precej abstrakten, npr. `sum` nima tipa `[Int] -> Int`, temveč `(Num a, Foldable t) => t a -> a`. Kadar ne potrebujemo najbolj splošne definicije (če delamo samo s seznami), bodo opozorila precej bolj berljiva, saj bodo specializirana za enostavne tipe.

Nekateri ste signature pisali za definicijo. To je bolje, kot če jih ne bi, a vseeno ni prav.

### Komentarji posameznih nalog

### `predzadnjiElement`

> Funkcija `predzadnjiElement` vrne predzadnji element seznama `l`.

Rešitev, napisana s funkcijami, ki ste jih poznali na prvih vajah, bi bila

    predzadnjiElement :: [a] -> a
    predzadnjiElement l = last $ init l

Ta rešitev je učinkovita, saj se zaradi tega, ker je Haskell len, čez seznam sprehodi le enkrat. V Pythonu bi podobno definirana funkcija bila precej neučinkovita, saj bi najprej vse elemente seznama `l` razen zadnjega prekopirala v nov seznam, nato pa še enkrat šla čez ta seznam, da bi dobila njegov zadnji element.

Nekateri ste pisali tudi `last(init(l))`, kar nakazuje, da ste navajeni Pythona, v katerem je argumente funkcij treba pisati v oklepajih. Oklepaji so v Haskellu namenjeni le temu, da določajo vrstni red. Tako je pri zgornji funkciji treba pisati `last (init l)`, saj se `last init l` sicer razume kot `(last init) l`. Ta zadnji izraz seveda nima smisla, saj je `init` funkcija, zato ne moremo vzeti njenega zadnjega elementa.

### `poisci`

> Funkcija `poisci` naj poišče `k`-ti element v seznamu `l`.
> Zgled: 
> 
>     ghci> poisci 2 [0,0,1,0,0,0]
>     1
>

Tu je najbolj običajna rešitev kar

    poisci :: Int -> [c] -> c
    poisci k l = l !! k

Če so vam všeč funkcije višjega reda, bi lahko napisali

    poisci = flip (!!)

Funkcija `flip` vzame funkcijo in vrne funkcijo z obrnjenima argumentoma. Če `(!!)` vzame seznam in indeks, potem `flip (!!)` vzame indeks in seznam. Tudi rešitev

    poisci k l = last (take k l)

bi tako kot pri prejšnji nalogi bila ustrezna zaradi tega, ker je Haskell len, v Pythonu pa bi bila precej neučinkovita.

### `podvoji`

> Funkcija `podvoji` naj 'podvoji' seznam `l`.
> Zgled:
> 
>     ghci> podvoji [1,2,3,3]
>     [1,1,2,2,3,3,3,3]
> 
> Namig: Funkcija `concat` iz seznama seznamov naredil seznam z elementi
> podseznamov.

Z uporabo obstoječih funkcij bi lahko napisali

    podvoji :: [a] -> [a]
    podvoji xs =  concat [[x, x] | x <- xs]

ali

    podvoji xs = [y | x <- xs, y <- [x, x]]


z rekurzijo pa bi lahko definirali

    podvoji [] = []
    podvoji (x:xs) = x : x : podvoji xs

ljubitelji funkcij višjega reda pa

    podvoji =  concatMap (\x -> [x, x])

Ena oddana rešitev je bila

    podvoji l = concat (take 2 (repeat l))

Ta ne deluje pravilno, saj ne podvoji posameznih elementov, vendar le stakne dve kopiji seznama, kar bi lahko napisali tudi z `l ++ l`.


### `zip'`

> Sestavite funkcijo `zip'`, ki naj deluje enako kot `zip`.
> Namig: Funkcija `min` vrne minimum dveh podanih števil.

To nalogo smo iz letošnjih vaj namenoma pobrisali, vendar vas jo je precej vseeno rešilo. Upam, da niste le prekopirali lanskih rešitev.

### `razdeli`

> Funkcija `razdeli k l` naj seznam `l` razdeli na dva seznama. V prvem naj bo prvi `k` elementov seznama `l`, v drugem pa vsi ostali. Funkcija naj vrne par teh dveh seznamov.
> Zgled:
> 
>     ghci> razdeli 3 [1,1,1,2,2,2]
>     ([1,1,1],[2,2,2])

Očitna rešitev je

    razdeli :: Int -> [a] -> ([a], [a])
    razdeli k l = (take k l, drop k l)

vendar se ta čez seznam zapelje dvakrat. Bolj učinkovito rešitev lahko napišemo rekurzivno:

    razdeli _ [] = ([], [])
    razdeli 0 xs = ([], xs)
    razdeli k (x:xs) =
        let (xs1, xs2) = razdeli (k - 1) xs in
        (x:xs1, xs2)

Nekateri ste namesto `drop k l` pisali `reverse $ take (length l - k) $ reverse l`. To sicer dela, ni pa najbolj učinkovito.

Pri tej nalogi izpeljani seznami niso bili potrebni, a ste nekateri (mogoče iz navade) pisali

    razdeli k l = ([a | a <- take k l], [a | a <- drop k l])

Izpeljani seznam `[a | a <- l]`, torej seznam vseh `a` iz `l`, je seveda enak `l`.

Nekateri ste tu uporabili tudi pomožne definicije:

    razdeli k l = (razdeli1 k l, razdeli2 k l)
    razdeli1 k l = take k l
    razdeli2 k l = drop k l

Če že uporabljate pomožne definicije, ki jih uporabljate le v eni definiciji, potem uporabite `where` in pišite

    razdeli k l = (razdeli1 k l, razdeli2 k l)
      where
        razdeli1 k l = take k l
        razdeli2 k l = drop k l

Seveda je funkcija `razdeli1` enaka `take`, funkcija `razdeli2` pa `drop`, zato pomožni definiciji nista potrebni.

Ena rešitev je bila tudi

    razdeli k l = [take k l, drop k l]

Ta program sicer ima tip, ker imata obe komponenti tip `[a]`, vendar ne iskanega. Imejte v glavi, da sezname uporabljamo takrat, kadar je število elementov poljubno, nabore pa takrat, kadar je število elementov vnaprej določeno. Če npr.
naloga zahteva, da vrnete par, so nabori prava rešitev tudi takrat, kadar imata obe komponenti isti tip.


### `zbrisi`

> Funkcija `zbrisi` naj iz seznama `l` pobriše `k`-ti element.
> Zgled:
> 
>     ghci> zbrisi 3 [0,0,0,1,0,0,0]
>     [0,0,0,0,0,0]

Lahko bi napisali

    zbrisi :: Eq a => Int -> [t] -> [t]
    zbrisi k l = take k l ++ drop (k + 1) l

samo to bi se čez seznam zopet zapeljalo dvakrat. Bolje bi bilo, če zapišemo

    zbrisi _ [] = []
    zbrisi 0 (x:xs) = xs
    zbrisi k (x:xs) = x : zbrisi (k - 1) xs

Tudi tu ste nekateri namesto `take k l ++ drop (k + 1) l` pisali `concat [take k l, drop (k + 1) l]`. Programa seveda delujeta enako, vendar takrat, ko stikamo le dva seznama, uporabimo prvo obliko, ker je bolj nazorna.

### `rezina`

> Funkcija `rezina i k l` naj sestavi novi seznam, ki naj vsebuje elemente seznama `l` od vključno `i`-tega do `k`-tega (brez `k`-tega).
> Zgled:
> 
>     ghci> rezina 3 6 [0,0,0,1,2,3,0,0,0]
>     [1,2,3]

Tu je najenostavnejša rešitev

    rezina :: Int -> Int -> [a] -> [a]
    rezina i k l = take (k - i) $ drop i l

### `vstavi`

> Napišite funkcijo `vstavi x k l`, ki na `k`-to mesto seznama `l` vrine `x`.
> Na primer:
> 
>     ghci> vstavi 2 5 [0,0,0,0,0,0]
>     [0,0,0,0,0,2,0]

Tu je potrebno poudariti, da `vstavi` vrne nov seznam in ničesar ne vrine v `l` – ta ostane tak, kot je.

Lahko bi pisali

    vstavi :: a -> Int -> [a] -> [a]
    vstavi x k l = take k l ++ [x] ++ drop k l

Vendar je to neučinkovito, saj se čez seznam `l` vozi trikrat - dvakrat, ko računa `take k l` in `drop k l`, tretjič pa še, ko stika `take k l` s preostankom. Bolj učinkovita je rekurzivna rešitev

    vstavi x _ [] = [x]
    vstavi x 0 ys = x : ys
    vstavi x k (y:ys) = y : vstavi x (k - 1) ys

### `zavrti`

> Napišite funkcijo `zavrti n l`, ki seznam `l` zavrti za `n` mest v levo.
> Na primer:
> 
>     ghci> zavrti 2 [1,2,3,4,5]
>     [3,4,5,1,2]

Lahko bi pisali

    zavrti :: Int -> [a] -> [a]
    zavrti n l = drop n l ++ take n l

Kar se čez seznam sprehodi trikrat. Malo bolj učinkovito je

    zavrti n l =
        let (l1, l2) = razdeli n l in
        l2 ++ l1

oziroma

    zavrti n = uncurry (flip (++)) . razdeli n

kar se čez seznam sprehodi dvakrat. Ugibam, da bi morala obstajati tudi rešitev, ki se čez seznam sprehodi enkrat. Mogoče jo najde kdo od vas?

### `pobrisi`

> Napišite funkcijo `pobrisi x l`, ki vrne seznam, ki je kot `l`,
> le da smo iz njega pobrisali vse pojavitve elementa `x`
> Na primer:
>
>     ghci> pobrisi 'a' "abrakadabra"
>     "brkdbr"

Z znanjem prvih predavanj bi lahko napisali

    pobrisi :: Eq a => a -> [a] -> [a]
    pobrisi x l = [y | y <- l, x /= y]

seveda pa je

    pobrisi x = filter (x /=)

### `jePalindrom`

> Funkcija `jePalindrom` naj ugotovi, če je seznam `l` palindrom.
> Zgled: 
> 
>     ghci> jePalindrom [1,2,3,2,1]
>     True
>     ghci> jePalindrom [1,2,3]
>     False

Najenostavneje je kar

    jePalindrom :: Eq a => [a] -> Bool
    jePalindrom l = l == reverse l

Pri dani implementaciji nizov z verižnimi seznami boljše možnosti ni, saj se ne moremo sprehajati od konca nazaj.

Nekateri ste pisali tudi

    jePalindrom l = if l == reverse l then True else False

Ta definicija je nepotrebna, saj bo rezultat pogojnega stavka `True` natanko takrat, ko bo vrednost `l == reverse l` enaka `True` in obratno za `False`. Če bi želeli napisati funkcijo `niPalindrom l`, prav tako ne bi napisali 

    niPalindrom l = if l == reverse l then False else True

temveč

    niPalindrom l = not $ l == reverse l

ali še bolje

    niPalindrom l = l /= reverse l


### `maxPoKomponentah`

> Napišite funkcijo `maxPoKomponentah l1 l2`, ki vrne seznam, ki ima za
> elemente večjega od elementov na ustreznih mestih v seznamih `l1` in `l2`.
> Če je eden od seznamov krajši, naj bo krajši tudi vrnjeni seznam.
> Na primer:
> 
>     ghci> maxPoKomponentah [1,10,5,6] [2,3,7,4,8]
>     [2,10,7,6]

Tu je najbolj direktno uporabiti izpeljane sezname in `zip`:

    maxPoKomponentah :: Ord a => [a] -> [a] -> [a]
    maxPoKomponentah l1 l2 = [max x y | (x, y) <- zip l1 l2]

če ste pa bolj zviti, pa lahko tudi

    maxPoKomponentah = zipWith max

Nekateri ste pozabili, da pare razstavljamo z vzorci, in ste napisali:

    maxPoKomponentah l1 l2 = [max (fst p) (snd p) | p <- zip l1 l2]

Nekatere rešitve so bile napisane tudi s pomočjo rekurzije, ki jo bom natančneje pokomentiral pri naslednjih vajah. Tu bi omenil le sledečo oddano rešitev

    maxPoKomponentah l1 l2  
        | l1 == [] || l2 == [] = []
        | otherwise = 
            [max (head l1) (head l2)] ++ maxPoKomponentah (tail l1) (tail l2)

Kadar z robnimi primeri v definicijah preverjamo, ali je argument dane oblike,
je bolje uporabiti vzorce, torej:

    maxPoKomponentah [] l2 = [] 
    maxPoKomponentah l1 [] = []
    maxPoKomponentah l1 l2 =
        [max (head l1) (head l2)] ++ maxPoKomponentah (tail l1) (tail l2)

V tem primeru lahko tudi napišemo

    maxPoKomponentah [] _ = [] 
    maxPoKomponentah _ [] = []
    maxPoKomponentah (x:xs) (y:ys) = [max x y] ++ maxPoKomponentah xs ys

Prav tako namesto `[el] ++ sez` raje pišimo `el : sez`, torej:

    maxPoKomponentah (x:xs) (y:ys) = max x y : maxPoKomponentah xs ys

### `drugiNajvecji`

> Napišite funkcijo `drugiNajvecji l`, ki vrne drugi največji element
> seznama `l`. Predpostavite lahko, da sta v `l` vsaj dva različna elementa.
> Na primer:
> 
>     ghci> drugiNajvecji [1,10,5,6]
>     6

Najbolj enostavna možnost je, da najprej poiščemo maksimum, nato pa še maksimum seznama brez najdenega maksimuma:

    drugiNajvecji :: Ord a => [a] -> a
    drugiNajvecji l = maximum $ pobrisi (maximum l) l

Če ste razumeli, da v primeru dveh enakih največjih elementov lahko vrnete enega od njiju, pa lahko napišete

    drugiNajvecji l = maximum $ pobrisi' (maximum l) l
      where
        pobrisi' x [] = []
        pobrisi' x (y:ys)
            | x == y = ys
            | otherwise = y : pobrisi' x ys
