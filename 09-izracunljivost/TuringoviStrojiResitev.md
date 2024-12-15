



# Turingovi stroji

## Naloga 1

**Definirajte k-tračni turingov stroj**

Enako kot eno-tračni turingov stroj, le da je tranzicijska funkcija oblike

$ Q \times \Gamma^k \to Q \times \Gamma^k \times \{L, R\}^k$

## Naloga 2

Dokažite, da lahko enotračni turingov stroj, kjer prehodna funkcija omogoča, da se glava ostane na mestu, simulira enotračni turingov stroj, kjer se glava vedno premakne levo ali desno.


Naj bo $M = (\Gamma, \square, Q, q_0, \delta)$ enotračni turingov stroj, kjer je $\delta$ funkcija oblike $\delta: Q \times \Gamma \to Q \times \Gamma \times \{L, N, R\}$.
Znak $N$ pomeni, da se glava ne premakne.

Ideja konstrukcije:
Edina stvar, ki jo res moramo spremeniti je $\delta$ funkcija.
Za tranzicije, ki premaknejo glavo levo ali desno, ni potrebno storiti ničesar.
Tiste, ki glave ne premaknejo, pa moramo spremeniti tako, da jih razdelimo na dve tranziciji, ki se izvedeta zaporedoma.
Prva glavo umakne (vseeno na katero stran) in stroj prestavi v novo stanje *vračanja*, druga tranzicija pa vrne glavo na mesto, kjer je bila na začetku in stroj vrne v stanje, v katerega bi se moral premakniti, če bi glava ostala na mestu.

Formalno:
$Q' = Q \cup \{\overline{q_i} \ | \ q_i \in Q\}$, vsakemu stanju $q_i$ priredimo njegovo "vračalno" stanje $\overline{q_i}$ - stroj je v vračanju in po tem, ko se vrne na mesto, kjer je bil, se vrne v stanje $q_i$. Ob prvem premiku že napišemo pravilen simbol na trak.

$$
\delta'(q, a) = \begin{cases}
(q', a', d) & \text{če } \delta(q, a) = (q', a', d) \land d \in \{L, R\} \ //\ \textrm{Če se premaknemo je vseeno}\\
(\overline{q'}, a', L) & \text{če } \delta(q, a) = (q', a', N) \ //\ \textrm{Se umaknemo in si zapomnimo kaj bo naslednje stanje}\\
(q', a, R) & \text{če } q = \overline{q'}  \ //\ \textrm{Če smo v stanju vračanja}\\
\end{cases}
$$

Očitno so tranzicije disjunktne.


## Naloga 3

Dan je jezik $J = \{ xyz | x, y, z \in \{0, 1\}^*, |x| = |y| = |z|, x \oplus y = z \}$.

1. Sestavite deterministični n-tračni turingov stroj, ki preveri, ali je dana beseda v jeziku $J$.

Ideja delovanja: 
Če imamo vse tri "podbesede" zapisane eno nad drugo lahko v linearnem času preverimo ali je tretja beseda res ekskluzivni ali prvih dveh.
Postopamo v več korakih. 
Najprej začetno besedo prepišemo na oba druga trakova in tako končamo z vsemi tremi glavami na koncu vhodne besede (na prvem praznem znaku). 
Glave nato začnemo pomikati nazaj levo v treh različnih konfiguracijah.
V prvi konfiguraciji se vse premaknejo levo, nato zgolj prvi dve, v tretji pa zgolj zgornja.
Tej konfiguraciji spet sledi prva, postopek ponavljamo, dokler prva glava na naleti na prazen znak.
Če v tem primeru ni v prvi konfiguraciji, besedo zavrnemo (njena dolžina ni večkratnik števila 3).
Sedaj je vsaka izmed glav na začetku ustrezne tretjine besede in lahko preprosto po vrsti preverjamo, ali znaki ustrezajo in končamo ko zadnja glava doseže prazni znak (s tem signaliziramo sprejemno stanje).


2. Sestavite deterministični enotračni turingov stroj, ki preveri, ali je dana beseda v jeziku $J$.

Ideja rešitve je podobna, le da zaradi omejitve na zgolj en trak, ločitve besed si sedaj "označimo" z uporabo dodatnih simbolov ("abcdef").
Besedo kopiramo tako, da se ustrezno premikamo levo in desno (enkrat ali dvakrat, odvisno od tega na kateri tretjini hočemo onačiti besedo).
Postopek kopiranja zahteva $O(n^2)$ korakov (čez celotno besedo dolgo $n$ znakov se sprehodimo največ $n$ krat).
Drugi del postopka pa je preverjanje ustreznosti take predelane besede.
TS se zapelje čez samo besedo in si s pomočjo stanj zapomni, kakšna mora biti veljavna konfiguracija $i-$ tega znaka
Že predelane znake označi z znaki "XYZ"\ in si s tem zapomni ustrezne že predelane znake.
Na ta način se glava čez celotno besedo sprehodi največ $n/3$ krat($+ c$, kjer je $c$ konstanta ki nastopa zaradi prehoda čez rob ustrezne besede (do konca besede gremo tako, da se sprehodimo čez konec in se vrnemo za en korak)), časovna zahtevnost drugega dela preverjanja je torej $O(n^2)$, skupna časovna zahtevnost pa $O(n^2)$.


## Naloga 4

Pokažite, da lahko vsak n-tračni turingov stroj simuliramo z enotračnim turingovim strojem.


Ideja: trakove napišemo enega za drugim, tako da vmes ločimo posamezne trakove z nekim posebnim simbolom. Vse trakove nato združimo v enega, tako da jih zlepimo skupaj.
Simbole, pod katerimi je navidezna glava na i-tem traku označimo z $\underline{x}$.

$\Sigma' = \Sigma \cup \{ \triangleright',  \triangleleft' , \underline{\triangleright'}, \underline{\triangleleft'}, \triangleright \}$

Trak zapišemo kot

$[\triangleright, \triangleright', x_1, \triangleleft', \triangleright', x_2,  \triangleleft', ...., \triangleright', x_3, \triangleleft', \triangleleft]$

Korak simuliramo tako, da se premikamo proti desni.
Ko pridemo do podčrtanega simbola opravimo korak za k-ti trak, nato pa se premaknemo na naslednji trak.
Po potrebi moramo vmesne poddtrakove podaljšati, če smo prepisali konec traku.