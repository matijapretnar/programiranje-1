---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: OCaml 4.11
  language: OCaml
  name: ocaml-jupyter
---

# Uvod v OCaml

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Zaradi vseh teh lastnosti veliko modernih programskih jezikov vpeljuje ideje iz funkcijskega programiranja. Da se bomo lahko osredotočili na te ideje, si bomo ogledali programski jezik OCaml, ki je bil eden prvih in je še danes eden najbolj popularnih funkcijskih jezikov.

## Delo z OCamlom

Poženimo prvi program v OCamlu:

```{code-cell}
let odgovor = min 8 7 * 6
```

Vidimo lahko več stvari:

- vrednosti definiramo s ključno besedo `let`
- OCaml je poleg končne vrednosti izračunal tudi njen tip `int`
- funkcije kličemo tako, da argumente naštejemo brez oklepajev
- pri tem ima uporaba funkcij (_aplikacija_) višjo prioriteto kot računske operacije

Vrednosti lahko definiramo tudi lokalno z izrazom `let ... in ...`

```{code-cell}
let odgovor =
    let prvi_delni_izracun = min 8 7 in
    let drugi_delni_izracun = 6 in
    prvi_delni_izracun * drugi_delni_izracun
```

V tem primeru bodo definicije na voljo v delu `in ...`, izven pa ne:

```{code-cell}
:tags: [raises-exception]

prvi_delni_izracun
```

Če želimo, lahko več lokalnih definicij hkrati podamo tako, da jih ločimo z `and`.

```{code-cell}
let odgovor =
    let prvi_delni_izracun = min 8 7
    and drugi_delni_izracun = 6
    in
    prvi_delni_izracun * drugi_delni_izracun
```

Razlika med tem in gnezdenimi `let ... in ...` je v tem, da so vrednosti definirane hkrati, zato se ne morejo nanašati ena na drugo:

```{code-cell}
let a = 6 in
let b = a + 1 in
a * b
```

```{code-cell}
:tags: [raises-exception]

let a = 6
and b = a + 1 in
a * b
```

## Vgrajeni tipi

Ena izmed največjih prednosti OCamla je njegov bogat in dovršen sistem tipov. Vsak pravilen program v OCamlu ima svoj tip, ki ga OCaml samodejno preveri pred vsakim izvajanjem, kar polovi ogromno napak.

### Tip `int`

Cela števila pripadajo tipu `int`, z njimi pa delamo podobno kot v Pythonu:

```{code-cell}
12 * (34 + 67) - 89
```

Za razliko od Pythona celoštevilsko delimo z `/`, ostanek pa izračunamo z `mod`:

```{code-cell}
22 / 7
```

```{code-cell}
22 mod 7
```

OCamlov tip `int` ne podpira poljubno velikih števil, zato lahko pri nekaterih operacijah pride do prekoračitve obsega:

```{code-cell}
4611686018427387902 + 1
```

```{code-cell}
4611686018427387902 + 2
```

### Tip `float`

Tipu `float` pripadajo števila s plavajočo vejico, ki jih pišemo kot v Pythonu, razlika pa se pojavi pri operacijah. Kot smo že omenili, OCaml preverja ustreznost tipov, in tako na primer operacija `*` sprejme dva argumenta tipa `int` in `int` tudi vrne. Če ji damo argumente tipa `float`, se bo OCaml pritožil, saj med tema dvema tipoma strogo loči:

```{code-cell}
:tags: [raises-exception]

2 * 3.141592
```

Operacije, ki sprejemajo števila s plavajočo vejico, se končajo s piko:

```{code-cell}
12.0 *. (34.0 +. 67.0) -. 89.0
```

```{code-cell}
22. /. 7.
```

```{code-cell}
let pi = 4. *. atan 1.
```

```{code-cell}
cos pi
```

### Tipa `string` in `char`

Nizi pripadajo tipu `string`, pišemo pa jih med dvojne narekovaje. Pogosta operacija na nizih je stikanje, ki ga pišemo kot `^`. Na voljo imamo tudi funkcije za pretvorbo v nize in iz nizov, pri čemer slednje lahko sprožijo napako:

```{code-cell}
"Programiranje " ^ string_of_int 1
```

```{code-cell}
6 * int_of_string "7"
```

```{code-cell}
:tags: [raises-exception]

6 * int_of_string "sedem"
```

Tipu `char` pripadajo posamezni znaki, ki jih pišemo med enojne narekovaje.

```{code-cell}
'a'
```

### Tip `bool`

Tipu `bool` pripadajo logične vrednosti, kjer imamo na voljo obe logični konstanti ter običajne logične operacije, pri čemer konjunkcijo pišemo kot `&&`, disjunkcijo pa kot `||`:

```{code-cell}
false && not (false || true)
```

Na voljo imamo tudi običajne relacije za primerjavo:

```{code-cell}
3 < 5 || 3 >= 5
```

Za primerjavo enakosti uporabljamo operaciji `=` in `<>`, ki argumente primerjata glede na vrednosti. Na voljo sta tudi primerjavi `==` in `!=`, ki gledata identičnost argumentov in ju uporabljamo le takrat, kadar smo v to popolnoma prepričani, saj nam sicer dajeta nepričakovane odgovore:

```{code-cell}
"A" == "A"
```

Dobili smo `false`, ker je OCaml naredil dva različna niza in ju shranil na dve različni mesti v pomnilniku. Če si naredimo en sam niz, pa enakost velja:

```{code-cell}
let a = "A"
```

```{code-cell}
a == a
```

Logične vrednosti lahko uporabljamo v pogojnih izrazih:

```{code-cell}
if 3 <> 5 then 10 else 20
```

Pogojni izrazi so lahko tudi vsebovani v drugih izrazih. Na primer, funkcija v spodnjem izrazu je rezultat pogojnega izraza:

```{code-cell}
(if 3 = 4 then cos else sin) pi
```

### Tipi naborov

Nabore v OCamlu pišemo med navadne oklepaje, komponente pa ločimo z vejico. Včasih lahko oklepaje tudi izpustimo, vendar tega raje ne počnimo.

```{code-cell}
(1, 2 < 3, cos pi)
```

```{code-cell}
1, 2, 3
```

Kot vidimo, imajo nabori tip označen s kartezičnim produktom <code>τ<sub>1</sub> * τ<sub>2</sub> * ... * τ<sub>n</sub></code>, kjer so <code>τ<sub>i</sub></code> tipi posameznih komponent. Naborov velikosti ena ni, ker niso potrebni, nabor velikosti 0 pa je natanko en:

```{code-cell}
()
```

Ker kartezičnega produkta nič tipov ne moremo zapisati, tip praznega nabora označujemo z `unit`.

Na parih (torej naborih velikosti 2) imamo na voljo funkciji `fst` in `snd`, ki projecirata na prvo in drugo komponento.

```{code-cell}
fst (5, true)
```

```{code-cell}
snd (5, true)
```

### Tip seznamov

Sezname v OCamlu pišemo med oglate oklepaje, vrednosti pa ločimo s podpičji. Vse vrednosti v seznamih morajo biti enakega tipa, seznam pa ima potem tip oblike <code>tip<sub>el</sub> list</code>, kjer je <code>tip<sub>el</sub></code> tip komponent.

```{code-cell}
[1; 2; 3; 4]
```

```{code-cell}
['a'; 'b'; 'c'; 'd']
```

Če se zatipkamo in namesto podpičij pišemo vejice, dobimo seznam z enim elementom, ki je nabor (spomnimo se, da lahko nabore pišemo brez oklepajev):

```{code-cell}
[1, 2, 3, 4]
```

Sezname sestavljamo z dvema operacijama. Z `::` sestavimo nov seznam z dano glavo in repom:

```{code-cell}
1 :: [2; 3; 4]
```

Kasneje bomo videli, da ima `::` prav posebno vlogo, saj je tako imenovani _konstruktor_ seznamov. Vsak neprazen seznam je namreč prek `::` sestavljen iz glave in repa. Tudi `[1; 2; 3; 4]` je v resnici samo okrajšava za `1 :: (2 :: (3 :: (4 :: []))))`.

Če želimo stakniti dva seznama, pa uporabimo funkcijo `@`:

```{code-cell}
[1; 2; 3] @ [4; 5; 6]
```

Za vajo lahko preverite, kateri izmed spodnjih seznamov so veljavni:

<details>
    <summary><code>[1; 2] :: [3; 4]</code></summary>
    NE
</details>
<details>
    <summary><code>1 :: 2 :: 3 :: []</code></summary>
    DA
</details>
<details>
    <summary><code>[1; 2] @ [3; 4]</code></summary>
    DA
</details>
<details>
    <summary><code>1 @ 2 @ [3]</code></summary>
    NE
</details>
<details>
    <summary><code>[1, 2] @ [3]</code></summary>
    NE
</details>
<details>
    <summary><code>1 :: 2 :: 3</code></summary>
    NE
</details>
<details>
    <summary><code>[1; 2] @ []</code></summary>
    DA
</details>
<details>
    <summary><code>[1; 2] :: []</code></summary>
    DA
</details>

### Tipi funkcij

Vsaka vrednost v OCamlu ima svoj tip, tudi funkcije. Tip funkcije je oblike <code>tip<sub>arg</sub> -> tip<sub>rez</sub></code>, kjer je <code>tip<sub>arg</sub></code> tip argumenta funkcije, <code>tip<sub>rez</sub></code> pa tip njenega rezultata. Na primer `string_of_int` vzame `int` in vrne `string`:

```{code-cell}
string_of_int
```

Podoben tip imajo tudi funkcije več argumentov (kaj točno ta tip predstavlja, pa bomo še videli):

```{code-cell}
atan2
```

## Definicije funkcij

Funkcije v OCamlu definiramo podobno kot vrednosti, le da za njihovim imenom naštejemo še imena argumentov:

```{code-cell}
let kvadriraj n = n * n
```

Funkcije potem uporabimo kot običajno:

```{code-cell}
kvadriraj 8
```

Na podoben način lahko definiramo funkcije več argumentov:

```{code-cell}
let zmnozi x y = x * y
```

```{code-cell}
zmnozi 6 7
```

Funkcije so lahko tudi argumenti drugih funkcij. Na primer, spodnja funkcija vzame funkcijo `f` ter jo dvakrat zaporedoma uporabi na `0`. Iz tega sledi, da mora `f` sprejeti argument tipa `int`. Ker rezultat `f 0` znova podamo `f`, mora tudi ta biti `int`, zato je tip funkcije `f` enak `int -> int`, kar OCaml sam izračuna:

```{code-cell}
let dvakrat_na_nic f = f (f 0)
```

```{code-cell}
dvakrat_na_nic succ
```

```{code-cell}
let podvoji_in_pristej_ena x = 2 * x + 1
```

```{code-cell}
dvakrat_na_nic podvoji_in_pristej_ena
```

Včasih majhnih funkcij kot je zgornja ni smiselno poimenovati. V tem primeru lahko pišemo anonimne funkcije oblike `fun arg -> ...`, na primer:

```{code-cell}
dvakrat_na_nic (fun x -> 2 * x + 1)
```

Anonimne funkcije lahko sprejmejo tudi več argumentov:

```{code-cell}
(fun x y -> x * y) 2 3
```

### Vzorci

V lokalnih definicijah in argumentih funkcij lahko argumente razstavimo s pomočjo vzorcev. Na primer, namesto:

```{code-cell}
let razdalja koord1 koord2 =
  let dx = fst koord1 -. fst koord2
  and dy = snd koord1 -. snd koord2
  in
  sqrt (dx ** 2. +. dy ** 2.)
```

lahko pišemo:

```{code-cell}
let razdalja koord1 koord2 =
  let (x1, y1) = koord1
  and (x2, y2) = koord2 in
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
```

ali še bolje kot:

```{code-cell}
let razdalja (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
```

Med večimi vzorci se lahko odločimo s pomočjo konstrukta `match`, ki sprejme več vej, ločenih z `|`, nato pa vrne prvo, ki ustreza danemu vzorcu. Na primer, namesto

```{code-cell}
let ustrezen_pozdrav ime =
  if ime = "Matija" then
    "Dober dan, gospod predavatelj!"
  else if ime = "Filip" || ime = "Žiga" then
    "Oj!"
  else
    "Dober dan, " ^ ime ^ "!"
```

raje pišemo:

```{code-cell}
let ustrezen_pozdrav ime =
  match ime with
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Filip" | "Žiga" -> "Oj!"
  | _ -> "Dober dan, " ^ ime ^ "!"
```

Z vzorci lahko razstavljamo tudi sezname, pri čemer lahko z `::` seznam razstavimo na glavo in rep. Pozor: `@` v vzorcih ne more nastopati, saj je funkcija in ne konstruktor.

```{code-cell}
let citiraj_knjigo avtorji naslov =
  match avtorji with
  | [] -> naslov
  | [avtor] -> avtor ^ ": " ^ naslov
  | prvi :: _ -> prvi ^ " in ostali: " ^ naslov
```

```{code-cell}
citiraj_knjigo [] "Skrivnosti podzemlja"
```

```{code-cell}
citiraj_knjigo ["Kos"; "Golob"] "Fizika 1"
```

Vzorce lahko tudi gnezdimo:

```{code-cell}
let za_lase_privlecena_funkcija = function
  | [] -> 0
  | [(x, _); (y, z)] -> x + y + z
  | ((_, x) :: _) -> 3 * x
```

```{code-cell}
za_lase_privlecena_funkcija []
```

```{code-cell}
za_lase_privlecena_funkcija [(1, 2)]
```

```{code-cell}
za_lase_privlecena_funkcija [(1, 2); (3, 4)]
```

Za funkcije, ki takoj izvedejo `match` na svojem zadnjem argumentu, lahko uporabimo bližnjico `function`:

```{code-cell}
let ustrezen_pozdrav = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Filip" | "Žiga" -> "Oj!"
  | ime -> "Dober dan, " ^ ime ^ "!"
```

Vsakič, ko pišemo `match` ali `function`, moramo biti pozorni na vrstni red vzorcev:

```{code-cell}
let neustrezen_pozdrav = function
  | ime -> "Dober dan, " ^ ime ^ "!"
  | "Matija" -> "Dober dan, gospod predavatelj!"
```

OCaml nas je opozoril, da druga veja nikoli ne bo uporabljena, saj bo prvi vzorec ustrezal vsem primerom.

```{code-cell}
neustrezen_pozdrav "Matija"
```

Prav tako nas OCaml opozori, če na kakšen primer pozabimo, saj bo ob izvajanju sicer lahko prišlo do napake:

```{code-cell}
let nepopoln_pozdrav = function
  | "Matija" -> "Dober dan, gospod predavatelj!"
  | "Filip" | "Žiga" -> "Oj!"
```

```{code-cell}
:tags: [raises-exception]

nepopoln_pozdrav "naključni študent"
```

### Rekurzivne funkcije

Če želimo definirati rekurzivno funkcijo, jo moramo podati z `let rec`:

```{code-cell}
let rec fakulteta = function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)
```

```{code-cell}
fakulteta 10
```

```{code-cell}
let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
```

Zgornja definicija je precej neučinkovita, zato si lahko pomagamo s pomožno funkcijo, ki deluje veliko hitreje:

```{code-cell}
let hitri_fib n =
  let rec aux n a b =
    if n = 0 then a else aux (n - 1) b (a + b)
  in aux n 0 1
```

Hkrati lahko definiramo tudi več rekurzivnih funkcij:

```{code-cell}
let rec je_sodo = function
  | 0 -> true
  | n -> je_liho (n - 1)

and je_liho = function
  | 0 -> false
  | n -> je_sodo (n - 1)
```

## Primerjava s Pythonom

Kljub temu da Python in OCaml podpirata precej podobnih konceptov (funkcije, pogojne stavke, sezname, nabore, tipe, ...), pa imata tudi kar nekaj razlik. Ena izmed njih je način, na katerega te konstrukte uporabljamo. OCaml je _deklarativni_ jezik, kar pomeni, da programe sestavljamo s pomočjo definicij vrednosti. Natančneje, OCaml je _funkcijski jezik_, saj pri sestavljanju ključno vlogo igrajo funkcije (poznamo tudi _logične_ deklarativne jezike, kot na primer Prolog, kjer vrednosti opisujemo s pogoji, ki računalnik vodijo do končne rešitve). Na primer, $10!$ v OCamlu najenostavneje izračunamo tako, da napišemo njeno matematično definicijo:

```ocaml
let rec fakulteta n =
  if n = 0 then 1 else n * fakulteta (n - 1)
in fakulteta 10
```

Kljub temu da tudi Python vsebuje prvine funkcijskega programiranja, pa je v osnovi _imperativni_ (zaradi vloge objektov pa tudi _objektni_) jezik, kjer računalnik usmerjamo prek zaporedja ukazov, ki začetno stanje spravijo v želeno stanje. Na primer, fakulteto bi izračunali tako, da bi si izbrali spremenljivko za hranjenje vrednosti, nato pa to spremenljivko postopoma spreminjali, dokler ne bi dobili iskanega števila:

```python
fakulteta = 1
for i in range(1, 11):
    fakulteta *= i
```

Ena izmed razlik med jezikoma je tudi v sistemu tipov. OCaml tipe preverja _statično_, torej še pred izvajanjem. Če napišemo:

```{code-cell}
:tags: [raises-exception]
let je_majhen x =
  if x < 10 then "Da" else false
```

dobimo napako, še preden funkcijo pokličemo. V nasprotju z OCamlom bo Python enako definicijo sprejel:

```python
def je_majhen(x):
    return 'Da' if x < 10 else False
```

vendar bo ta v nekaterih primerih delala uspešno, v drugih pa ne:

```python
>>> je_majhen(3) + '!'
'Da!'

>>> je_majhen(10) + '!'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s)
  for +: 'bool' and 'str'
```

Kot vidimo v opozorilu, je Python tipe sicer preveril (zato računalnik ni na slepo delal z ničlami in enicami, kar bi lahko vodilo do hujših napak), vendar šele takrat, ko smo program izvedli. Pravimo, da ima Python _dinamičen_ sistem tipov.

Prav tako so OCamlovi tipi precej bogatejši, na primer za spodnji seznam in funkcijo OCaml zelo natančno pove, kakšne oblike sta:

```{code-cell}
[(1, ['a']); (10, []); (0, ['x'; 'y'])]
```

```{code-cell}
fun x -> [(x ^ "!", 0)]
```

Python pa sporoči le to, da sta seznam in funkcija:

```python
>>> type([(1, ['a']), (10, []), (0, ['x', 'y'])])
<type 'list'>

>>> type(lambda x: [(x + '!', 0)])
<type 'function'>
```

```{code-cell}

```

## Polimorfizem

Vsaka vrednost v OCamlu ima natančno določen tip. Kakšen pa je tip funkcije `@`, saj lahko z njo stikamo tako sezname logičnih vrednosti, sezname števil, sezname seznamov števil, ...

```{code-cell}
[true; false] @ [false; true]
```

```{code-cell}
[1; 2] @ [3; 4; 5]
```

```{code-cell}
[[1]] @ [[2; 3]; [4; 5]]
```

Je `@` torej tipa `bool list -> bool list -> bool list` ali `int list -> int list -> int list` ali `int list list -> int list list -> int list list`? V resnici je lahko tipa `α list -> α list -> α list` za poljuben tip `α`. To v OCamlu označimo kot `'a list -> 'a list -> 'a list`. In res:

```{code-cell}
(@)
```

Vrednostim, ki imajo v tipih spremenljivke, pravimo _parametrično polimorfne_. Polimorfne zaradi tega, ker lahko delujejo pri več tipih, parametrično pa zato, ker pri vseh tipih delujejo na enak način. Poznamo tudi tako imenovani _ad hoc_ polimorfizem, kjer pri nekaterih tipih funkcije delujejo na en način, pri nekaterih na drugačen, pri tretjih pa sploh ne. Primer takega polimorfizma je na primer `+` v Pythonu: števila sešteva, sezname in nize stika, na funkcijah pa ne deluje. OCaml ad hoc polimorfizma nima, ker povzroča težave pri določanju tipov.

V parametrično polimorfnih funkcijah lahko nastopa več parametrov. Na primer, projekcija na prvo komponento vzame par iz kartezičnega produkta poljubnih dveh tipov in slika v prvega:

```{code-cell}
fst
```

Tudi nekatere vrednosti so parametrično polimorfne:

```{code-cell}
[]
```

```{code-cell}
([], [[]], ([], 3))
```

Seveda pa so najbolj koristne polimorfne funkcije, na primer:

```{code-cell}
let rec dolzina =
  function
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```

```{code-cell}
dolzina [10; 20; 30]
```

```{code-cell}
let rec preslikaj f =
  function
  | [] -> []
  | x :: xs -> f x :: preslikaj f xs
```

```{code-cell}
preslikaj succ [10; 20; 30]
```
