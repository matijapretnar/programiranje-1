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

# Funkcije

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Pri konstruktih OCamla, ki smo jih spoznali do sedaj, morda presenetljivo ni bilo nobene omembe zank, ki so eno osnovnih orodij v Pythonu. :


```ocaml
let rec dolzina = function
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```


```ocaml
dolzina [1; 2; 3]
= 1 + dolzina [2; 3]
= 1 + (1 + dolzina [3])
= 1 + (1 + (1 + dolzina []))
= 1 + (1 + (1 + 0))
= 1 + (1 + 1)
= 1 + 2
= 3
```



```ocaml
let rec dolzina' acc = function
  | [] -> acc
  | _ :: xs -> dolzina' (acc + 1) xs
```

Kot vidimo, dolžino računamo sproti, zato sklad ne raste:

```ocaml
dolzina' 0 [1; 2; 3]
= dolzina' (0 + 1) [2; 3]
= dolzina' 1 [2; 3]
= dolzina' (1 + 1) [3]
= dolzina' 2 [3]
= dolzina' (2 + 1) []
= dolzina' 3 []
= 3
```

```{code-cell}
let rec f x y =
    if y = 0 then x else f (x + 1) (y - 1)
```

```{code-cell}
f 0 1000
```

```{code-cell}
f 0 100000
```

```{code-cell}
f 0 10000000
```

Za razliko od OCamla Python repnih klicev ne optimizira, saj zaradi lažjega odpravljanja napak želi ohraniti _traceback_, torej sled klicev, ki je vodila do dane točke v izvajanju:

```python
def f(x, y):
    if y == 0:
        return x
    else:
        return f(x + 1, y - 1)
```

```python
>>> f(0, 1000)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 5, in f
  ...
  File "<stdin>", line 2, in f
RecursionError: maximum recursion depth exceeded
```

## Zlaganje seznamov


```{code-cell}
zlozi_desno (+) [1; 2; 3; 4; 5] 0
```

Za vajo lahko na enak način izrazite še ostale funkcije `produkt`, `dolzina` in `preslikaj`. Funkcija `zlozi_desno` je seveda zelo uporabna, zato je na voljo tudi v standardni knjižnici in sicer pod imenom `List.fold_right`.

```{code-cell}
List.fold_right
```


```ocaml
zlozi_levo f z [1; 2; 3]
= zlozi_levo f (f z 1) [2; 3]
= zlozi_levo f (f (f z 1) 2) [3]
= zlozi_levo f (f (f (f z 1) 2) 3) []
= (f (f (f z 1) 2) 3)
```


```{code-cell}
let rec zlozi_levo f z xs =
  match xs with
  | [] -> z
  | x :: xs -> zlozi_levo f (f z x) xs
```

Za razliko od `List.fold_right` je tudi repno rekurzivna.

```{code-cell}
List.fold_left
```

```{code-cell}
zlozi_levo (^) "X" ["A"; "B"; "C"]
```

```{code-cell}
zlozi_levo (+) 0 [10; 20; 30]
```

```{code-cell}
zlozi_levo (fun xs x -> x :: xs) [] [1; 2; 3]
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

OCaml tipe tudi sam izračuna. Na primer, spodnja funkcija vzame funkcijo `f` ter jo dvakrat zaporedoma uporabi na `0`. Iz tega sledi, da mora `f` sprejeti argument tipa `int`. Ker rezultat `f 0` znova podamo `f`, mora tudi ta biti `int`, zato je tip funkcije `f` enak `int -> int`, kar OCaml sam izračuna:

```{code-cell}
let dvakrat_na_nic f = f (f 0)
```

```{code-cell}
dvakrat_na_nic succ
```