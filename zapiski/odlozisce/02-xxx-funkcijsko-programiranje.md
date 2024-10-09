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

## Delo z OCamlom

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

OCamlov tip `int` ne podpira poljubno velikih števil, zato lahko pri nekaterih operacijah pride do prekoračitve obsega:

```{code-cell}
4611686018427387902 + 1
```

```{code-cell}
4611686018427387902 + 2
```

### Tip `bool`

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


```{code-cell}
if 3 <> 5 then 10 else 20
```

Pogojni izrazi so lahko tudi vsebovani v drugih izrazih. Na primer, funkcija v spodnjem izrazu je rezultat pogojnega izraza:

```{code-cell}
(if 3 = 4 then cos else sin) pi
```

### Tipi naborov

 Včasih lahko oklepaje tudi izpustimo, vendar tega raje ne počnimo.

```{code-cell}
(1, 2 < 3, cos pi)
```

```{code-cell}
1, 2, 3
```

Naborov velikosti ena ni, ker niso potrebni, nabor velikosti 0 pa je natanko en:

```{code-cell}
()
```

Ker kartezičnega produkta nič tipov ne moremo zapisati, tip praznega nabora označujemo z `unit`.


### Tip seznamov

Če se zatipkamo in namesto podpičij pišemo vejice, dobimo seznam z enim elementom, ki je nabor (spomnimo se, da lahko nabore pišemo brez oklepajev):

```{code-cell}
[1, 2, 3, 4]
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


## Definicije funkcij

Na primer, spodnja funkcija vzame funkcijo `f` ter jo dvakrat zaporedoma uporabi na `0`. Iz tega sledi, da mora `f` sprejeti argument tipa `int`. Ker rezultat `f 0` znova podamo `f`, mora tudi ta biti `int`, zato je tip funkcije `f` enak `int -> int`, kar OCaml sam izračuna:

```{code-cell}
let dvakrat_na_nic f = f (f 0)
```

```{code-cell}
dvakrat_na_nic succ
```

Anonimne funkcije lahko sprejmejo tudi več argumentov:

```{code-cell}
(fun x y -> x * y) 2 3
```

### Vzorci

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

### Rekurzivne funkcije

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
