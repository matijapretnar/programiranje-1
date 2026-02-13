# 5. domača naloga

Pri tej domači nalogi boste rešili nekaj računskih problemov s pomočjo metod, ki smo jih spoznali na predavanjih. Vse naloge rešujte v OCamlu, vsako rešitev pa **dobro dokumentirajte**, da bo iz nje razvidna pravilnost ter časovna zahtevnost rešitve.

Pri vsaki nalogi bomo ocenjevali učinkovitost, preglednost in eleganco rešitve ter natančnost, razumljivost in pravilnost spremnega besedila.

## Deli in vladaj

Rešiti morate dve nalogi, ki se rešujeta z metodo deli in vladaj. Rešite eno izmed prvih dveh in eno izmed drugih dveh nalog.

### 1. Največja vsota podseznama

Dan naj bo neurejen seznam celih števil $[a_1, \dots, a_n]$. Napišite algoritem, ki v času $O(n \log n)$ poišče največjo možno vsoto strnjenega podseznama $[a_i, \dots, a_j]$.

Primer vhodnih podatkov: $[3, -9, 6, 10, -4, -2, 7]$.\
Rešitev primera: $17$.

### 2. Obzorje mesta

Dan naj bo seznam stavb mesta $[[L_1, D_1, V_1], \ldots, [L_n, D_n, V_n]]$, kjer vsaka trojka predstavlja stavbo v mestu, ki je opisana z levo ($L_i$) in desno ($D_i$) koordinato, kjer se začne in konča (koordinati sta vključeni) in višino ($V_i$).

Vaša naloga je napisati algoritem, ki v času $O(n \log n)$ izračuna obzorje mesta, ki ga opazovalec vidi - torej največje višine stavb po vsej širini mesta. Obzorje predstavlja seznam ključnih točk, kjer se višina obzorja spremeni.

Primer vhodnih podatkov: $[[2, 9, 10],[3, 7, 15],[5, 12, 12]]$.\
Rešitev primera: $[[2, 10], [3, 15], [8, 12], [13, 0]]$.

### 3. Štetje števila inverzij

Dan naj bo seznam celih števil $[a_1, \dots, a_n]$. Inverzija je par indeksov $(i, j)$, za katerega velja $i < j$ in $a_i > a_j$. Napišite algoritem, ki v času $O(n \log n)$ prešteje skupno število vseh inverzij v seznamu.

Primer vhodnih podatkov: $[3, 1, 2, 5, 7, 4]$.\
Rešitev primera: $4$.

### 4. Lokalni minimum matrike

Dana je matrika $M$ velikosti $n \times m$, kjer so vsa števila različna. Element $M[i, j]$ je lokalni minimum, če je manjši od vseh svojih neposrednih sosedov (levo, desno, zgoraj in spodaj). Napišite algoritem, ki v času $O(n \log m)$ (ali $O(n + m)$) poišče poljuben lokalni minimum v matriki. Polja izven matrike obravnavajte kot večja od vseh v matriki (neskončno velika).

Primer vhodnih podatkov: ​$[[40,11,35,8,85],[50,14,45,12,55],[60,22,15,3,95],[70,33,75,80,88]]$.\
Rešitve primera (vaš program mora najti eno izmed): $11, 8, 3$.

## Dinamično programiranje

Iz spodnjega seznama nalog na straneh [Project Euler](https://projecteuler.net/), [Advent of Code](https://adventofcode.com/) in [Univerzitetni programerski maraton](https://putka-upm.acm.si/tasks/s/list/) rešite naloge, skupaj vredne vsaj 8 točk:

- [#67 Maximum Path Sum II](https://projecteuler.net/problem=67): 1 točka
- [#76 Counting Summations](https://projecteuler.net/problem=76): 1 točka
- [#164 Three Consecutive Digital Sum Limit](https://projecteuler.net/problem=164): 2 točki
- [2020, Day 10: Adapter Array](https://adventofcode.com/2020/day/10): 2 točki
- [#169 Sums of Powers of Two](https://projecteuler.net/problem=169): 3 točke
- [Finale 2021: Predmetni moduli](https://putka-upm.acm.si/tasks/t/predmetni-moduli/): 3 točke
- [#215 Crack-free walls](https://projecteuler.net/problem=215): 4 točke
- [2023, Day 12: Hot Springs](https://adventofcode.com/2023/day/12): 4 točke
- [1. kolo 2020: Trgovec s kriptovalutami](https://putka-upm.acm.si/tasks/t/kriptotrgovec/): 4 točke
- [#534 Weak Queens](https://projecteuler.net/problem=534): 5 točk

Na vseh straneh lahko do nalog dostopate brez spletne prijave, le ta pa vam bo dovolila oddajo [1] rešitve ali rezultata in preverjanje pravilnosti. Za rešeno nalogo portala *Advent of Code* morate rešiti *oba dela naloge* - drugi del naloge se odklene, ko oddate pravilni odgovor na vprašanje prvega dela, zato je v tem primeru spletna prijava obvezna.

Opomba:
- [1] Spletni portal *Putka* preko katerega poteka *Univerzitetni programerski maraton* ne podpira oddaje rešitev v Ocaml datotekah, v primeru, da vas zanimajo testni primeri na strani, mi pišite (Nejc).
