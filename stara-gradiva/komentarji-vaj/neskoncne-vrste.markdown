
## Neskončne vrste

    
    -- Polinomi cont'd
    -- ===============
    
    -- Vzemite polinom s prejšnjih vaj in preverite, če deluje:
    
    -- (1 + x)^5.
    
    -- eksponentna = 1 + integral eksponentna
    

Prvi del vam ni delal težav, drugi pa je malo bolj zapleten. Zakaj? Če želimo imeti rekurzivno definicijo neskončne vrste, morajo biti ta definicija *produktivna*, torej da proizvede vsaj en člen, preden pogleda samo sebe. Na primer, seznam `naravna = 1 : 2 : 3 : ...` zadošča tako enakosti `naravna = naravna` kot `naravna = 1 : map succ naravna`, vendar je le druga definicija produktivna, saj gre kot

    naravna
    = 1 : map succ naravna
    = 1 : map succ (1 : map succ naravna)
    = 1 : 2 : map succ (map succ naravna)
    = ...

prva pa se zacikla. Pri integralih je treba biti malo bolj previden. Če pri definicijah, kot sem jih povedal pri [komentarjih 4. vaj](https://ucilnica.fmf.uni-lj.si/mod/forum/discuss.php?d=6051), definiramo
    
    eksponentna = 1 + integral eksponentna

se bo definicija zaciklala. Težavi sta dve. Prvič smo pisali

    polinom :: [Rational] -> Polinom
    polinom = Polinom . reverse . dropWhile (== 0) . reverse
    
    integral :: Polinom -> Polinom
    integral (Polinom koef) = polinom $ 0 : zipWith (/) koef [1..]

torej `integral` kliče funkcijo `polinom`, ki vmes seznam obrne na glavo, da se znebi odvečnih ničel s konca. Predstavljate si lahko, da se to pri neskončnih vrstah ne bo končalo dobro. Oziroma sploh končalo. Tudi če pametni konstruktor povsod (tudi v `(+)`, `(-)`, `(*)`, …) nadomestimo z neumnim `Polinom`:

    integral (Polinom koef) = Polinom $ 0 : zipWith (/) koef [1..]

definicija ne bo delala. Zakaj? Izračunajmo, koliko je `eksponentna`

    eksponentna
    = 1 + integral eksponentna
    = 1 + integral (1 + eksponentna)
    = 1 + integral (1 + integral (1 + eksponentna))
    = ...

Druga težava je namreč v tem, da bo funkcija `integral`, preden bo izvrgla prvi člen `0`, svoj argument računala toliko časa, dokler ne bo oblike `Polinom koef`, torej v neskončnost. Če pa definicijo zapišemo malo drugače:

    integral poli = Polinom $ 0 : zipWith (/) (koeficienti poli) [1..]

kjer je funkcija `koeficienti` definirana kot

    koeficienti :: Polinom -> [Rational]
    koeficienti (Polinom koef) = koef

pa so zadeve videti podobne, vendar so ključno drugačne. Račun tedaj gre kot:

    eksponentna
    = 1 + integral eksponentna
    = 1 + Polinom $ 0 : zipWith (/) (koeficienti eksponentna) [1..] = ...
    = Polinom $ 1 : zipWith (/) (koeficienti eksponentna) [1..] = ...
    = Polinom $ 1 : zipWith (/) (1 : zipWith (/) (koeficienti eksponentna) [1..]) = ...
    = Polinom $ 1 : 1 : ...

Torej, `integral` ne gre odvijati definicije funkcije `eksponentna`, temveč takoj začne s koeficientom `0`. Šele ko se računa drugi koeficient, želi funkcija `koeficienti` odviti definicijo, ki pa tedaj že ima obliko `Polinom (1 : ...)`. Kot vidite, znajo biti neskončne rekurzivne definicije malo bolj zvite, zato je treba biti zelo previden.

