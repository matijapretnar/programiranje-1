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

# Funkcije

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Pri konstruktih OCamla, ki smo jih spoznali do sedaj, morda presenetljivo ni bilo nobene omembe zank, ki so eno osnovnih orodij v Pythonu. V resnici OCaml podpira tudi zanke, vendar jih bomo spoznali kasneje. Namesto njih običajno uporabljamo rekurzivne funkcije, zato se moramo naučiti, kako take funkcije napišemo učinkovito. Pravimo, da je klic funkcije _repen_, če se izvede kot zadnji korak v definiciji funkcije. Najprej si oglejmo nasproten primer: funkcije, ki drugo funkcijo uporabijo v pomožnem izračunu:

```ocaml
let f x = 4 * x
let g x = 6 + f x
let h x = 3 * g x
```

Ker klici niso repni, morajo klicajoče funkcije svoje delo odložiti na tako imenovani sklad in počakati na klicane funkcije, preden lahko nadaljujejo z delom:

```ocaml
h 2                (* h dela *)
= 3 * g 2            (* g dela, h čaka *)
= 3 * (6 + f 2)      (* f dela, g in h čakata *)
= 3 * (6 + (4 * 2))  (* f dela, g in h čakata *)
= 3 * (6 + 8)        (* g dela, h čaka *)
= 3 * 14             (* h dela *)
= 42
```

Zdaj pa si oglejmo še tri funkcije, v katerih so klici pomožnih funkcij repni:

```ocaml
let f x = 3 * x
let g x = f (6 + x)
let h x = g (4 * x)
```

V tem primeru lahko klicajoča funkcija ob klicu delo v celoti prepusti klicani funkciji, zato ni ničesar potrebno odlagati na sklad:

```ocaml
h 2                (* h dela *)
= g (4 * 2)          (* h dela *)
= g 8                (* g dela *)
= f (6 + 8)          (* g dela *)
= f 14               (* f dela *)
= 3 * 14             (* f dela *)
= 42
```

Repni klici so še posebej pomembni pri rekurzivnih funkcijah, pri katerih je število klicev lahko zelo veliko. Na primer, oglejmo si funkcijo za izračun dolžine seznama, ki deluje tako, da izračuna dolžino repa, _po tem_ pa rezultatu prišteje še ena:

```ocaml
let rec dolzina = function
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```

Če izračunamo dolžino seznama `[1; 2; 3]`, se bo funkcija rekurzivno poklicala trikrat in vsakem klicu bo svoje delo odložila na sklad:

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

Če bi bil seznam dolg, bi to lahko vodilo do _prekoračitve sklada_ (_stack overflow_). Za razliko od Pythona, kjer je velikost sklada nastavljena na globino okoli 1000 klicev, je v OCamlu velikost nastavljena na globino okoli 100000, kar je za precej primerov dovolj. Še bolje pa je, da funkcijo napišemo tako, da bodo rekurzivni klici repni. Taki funkcij pravimo _repno rekurzivna_. Funkcijo za izračun dolžine bi lahko napisali repno rekurzivno tako, da ji podamo še dodaten argument, v katerem si bomo podajali dolžino do sedaj pregledanega repa. Take vrste argumentu dostikrat pravimo _akumulator_ (ker nabira do sedaj izračunani rezultat).

```ocaml
let rec dolzina' acc = function
  | [] -> acc
  | _ :: xs -> dolzina' (acc + 1) xs
```

Kot vidimo, dolžino računamo sproti, zato sklad ne raste:

```ocaml
dolzina' 0 [1; 2; 3]
= dolzina' (1 + 0) [2; 3]
= dolzina' 1 [2; 3]
= dolzina' (1 + 1) [3]
= dolzina' 2 [3]
= dolzina' (2 + 1) []
= dolzina' 3 []
= 3
```

OCaml repne klice prepozna in jih optimizira, zato lahko repno rekurzivne funkcije kličemo na poljubno velikih argumentih:

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

## Funkcije višjega reda

Kadar funkcija za argument sprejme drugo funkcijo, govorimo o funkcijah višjega reda. Na primer funkcija `trikrat` sprejme funkcijo `f` in argument `x` ter `f` trikrat zaporedoma uporabi na njem:

```{code-cell}
let trikrat f x = f (f (f x))
```

```{code-cell}
trikrat succ 39
```

```{code-cell}
trikrat sqrt 16.
```

Običajne funkcije med števili, nizi, seznami in podobnimi vrednostmi so funkcije prvega reda. Funkcija $n$-tega reda pa je taka, ki za argument sprejme funkcijo $n - 1$-tega reda. Pri tem predmetu bomo spoznali tudi funkcije tretjega reda, velika večina funkcij, s katerimi se bomo ukvarjali, pa bodo funkcije prvega ali drugega reda.

## Standardna knjižnica

Spoznali smo že funkcijo `preslikaj`, ki vzame funkcijo in jo uporabi na vseh elementih seznama. To in še mnogo drugih podobnih funkcij, ki so zelo uporabne pri pisanju programov, najdemo v [standardni knjižnici]((http://caml.inria.fr/pub/docs/manual-ocaml/libref/)), ki je razdeljena na module, na primer:

- osnovni modul [`Stdlib`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.html), ki je vedno naložen in ga ni treba posebej navajati,
- modul [`String`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html) za delo z nizi,
- modul [`List`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html) za delo s seznami ali
- modul [`Random`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html) za delo s psevdonaključnimi vrednostmi.

Do funkcij v modulih dostopamo s klici oblike `Modul.funkcija`. Na primer, funkcija `preslikaj` je dostopna kot:

```{code-cell}
List.map
```

```{code-cell}
List.map String.length ["To"; "je"; "en"; "seznam"]
```

## Anonimne funkcije

Recimo, da iz seznama želimo pobrisati vse pojavitve določenega elementa. To enostavno naredimo s funkcijo `List.filter`, ko obdrži vse elemente, ki zadoščajo danemu predikatu:

```{code-cell}
let pobrisi x sez =
  let je_razlicen_od_x y = x <> y in
  List.filter je_razlicen_od_x sez
```

```{code-cell}
pobrisi 10 [1; 2; 10; 5; 10; 99]
```

Precejšen del funkcije je bil namenjen definiciji in klicu pomožne funkcije `je_razlicen_od_x`, ki pa je v svojem bistvu precej enostavna. Veliko pisanja si prihranimo z uporabo _anonimnih_, torej nepoimenovanih funkcij, ki so oblike `fun x -> ...`, kar ustreza matematičnemu predpisu $x \mapsto \cdots$. Na primer, zgornjo funkcijo bi lahko napisali preprosto kot:

```{code-cell}
let pobrisi' x sez =
  List.filter (fun y -> x <> y) sez
```

```{code-cell}
pobrisi' 10 [1; 2; 10; 5; 10; 99]
```

V anonimnih funkcijah lahko prav tako uporabljamo vzorce, na primer:

```{code-cell}
List.map (fun (x, y) -> x + y) [(1, 1); (2, 10); (-3, 5)]
```

## Curryirane funkcije

Vzemimo enostavno funkcijo dveh argumentov:

```{code-cell}
let zmnozi x y = x * y
```

```{code-cell}
zmnozi 2 3
```

Poglejmo, kaj se zgodi, če ji podamo samo enega. Pričakovali bi napako, saj se že Python pritoži, če funkciji podamo napačno število argumentov. OCaml pa se ne pritoži, temveč javi, da je izračunal funkcijo tipa `int -> int`.

```{code-cell}
zmnozi 2
```

Dobljeno funkcijo poimenujmo, da bomo lahko videli, kaj počne:

```{code-cell}
let f = zmnozi 2
```

```{code-cell}
f 3
```

```{code-cell}
f 10
```

```{code-cell}
f 50
```

Videti je, kot da funkcija `f` podvoji svoj argument oziroma množi z $2$. Naše opažanje je pravilno. Funkcije dveh argumentov so namreč le funkcije _enega_ argumenta (prvega), ki vrnejo funkcijo _enega_ argumenta (drugega). Ko ta funkcija dobi še drugi argument ima na voljo vse, kar potrebuje, in lahko izračuna končno vrednost. Na primer, ob klicu `zmnozi 2` funkcija `zmnozi` dobi prvi argument, vendar potrebuje še drugega, da lahko izračuna produkt. Ko ga dobi, ga pomnoži s poprej podanim argumentom `2` in vrne rezultat. V resnici nam `zmnozi 2` ni treba shraniti v `f`, temveč lahko direktno pišemo:

```{code-cell}
(zmnozi 2) 3
```

```{code-cell}
(zmnozi 2) 10
```

in dobimo enak rezultat.

Tako je definicija funkcija več argumentov `fun x y -> ...` v resnici samo okrajšava za gnezdeni funkciji enega argumenta `fun x -> fun y -> ...`.

V resnici je `f x y` samo okrajšava za `(f x) y`. Podobno je `f x y z` okrajšava za `((f x) y) z`. Pravimo, da je aplikacija _levo asociativna_. (Če bi želeli, bi se lahko odločili, da bi bila aplikacija desno asociativna, torej da bi `a b c` pomenilo `a (b c)`, kar bi bilo koristno za veriženje funkcij, saj bi namesto `f (g x)` lahko pisali kar `f g x`. Vendar je takih primerov veliko manj kot klicev funkcij več argumentov, zato je leva asociativnost boljša izbira).

Oglejmo si še funkcijski tip `A -> B -> C`. Funkcija tega tipa sprejme dva argumenta: najprej sprejme argument tipa `A`, nato pa vrne funkcijo, ki čaka še drugi argument tipa `B`. Torej je `A -> B -> C` okrajšava za `A -> (B -> C)`. Podobno je `A -> B -> C -> D` okrajšava za `A -> (B -> (C -> D))`. Operacija `->` ki iz dveh tipov vrne tip funkcij med njima je torej _desno_ asociativna. (Tudi tu bi se lahko odločili, da je `->` levo asociativna operacija, s čimer bi bil tip `A -> B -> C` okrajšava za funkcijo drugega reda `(A -> B) -> C` - ker je uporabnih funkcij višjega veliko manj kot funkcij več argumentov, je desna asociativnost boljša izbira).

Takim funkcijam več argumentov pravimo, da so _curryirane_. Imenujejo se po logiku [Haskellu Curryju](https://en.wikipedia.org/wiki/Haskell_Curry), ki je bil eden prvih raziskovalcev idej, na katerih temeljijo funkcijski jeziki. Namesto curryiranih funkcij bi za funkcije več argumentov lahko uporabili tudi običajne funkcije, ki sprejmejo par:

```{code-cell}
let zmnozi' (x, y) = x * y
```

```{code-cell}
zmnozi' (2, 3)
```

```{code-cell}
zmnozi' (2, 10)
```

V tem primeru jih seveda ne moremo uporabiti samo na enem argumentu:

```{code-cell}
:tags: [raises-exception]
zmnozi' 2
```

Prednost Curryiranih funkcij je, da jih lahko uporabimo delno, nato pa dobljeno funkcijo preostalih argumentov uporabimo v kakšni funkciji drugega reda:

```{code-cell}
List.map (zmnozi 2) [10; 20; 30]
```

```{code-cell}
List.map (List.map succ) [[10; 20]; [30]]
```

Seveda pa sta tipa `A * B -> C` in `A -> B -> C` izomorfna, saj vsebujeta vsebinsko enake, le po obliki različne funkcije. Med njima obstaja izomorfizem, tako kot med množicama $C^{A \times B} \cong (C^B)^A$. Postopku pretvorbe iz običajne funkcije v curryirano pravimo _curryiranje_:

```{code-cell}
let curry f = fun x y -> f (x, y)
```

```{code-cell}
let zmnozi'' = curry zmnozi'
```

```{code-cell}
List.map (zmnozi'' 2) [10; 20; 30]
```

Kot vsak izomorfizem ima tudi curryiranje svoj inverz:

```{code-cell}
let uncurry g = fun (x, y) -> g x y
```

## Zlaganje seznamov

Ko v OCamlu napišemo nekaj funkcij na seznamih, vidimo, da imajo dostikrat podobno obliko: imajo osnoven primer za prazen seznam ter rekurzivnega za sestavljen seznam. Na primer, vsoto definiramo kot:

```{code-cell}
let rec vsota xs =
  match xs with
  | [] -> 0
  | x :: xs -> x + vsota xs
```

in izračunamo kot:

```ocaml
vsota [1; 2; 3]
= 1 + vsota [2; 3]
= 1 + (2 + vsota [3])
= 1 + (2 + (3 + vsota []))
= 1 + (2 + (3 + 0))
```

Produkt definiramo kot:

```{code-cell}
let rec produkt xs =
  match xs with
  | [] -> 1
  | x :: xs -> x * produkt xs
```

in izračunamo kot:

```ocaml
produkt [1; 2; 3]
= 1 * produkt [2; 3]
= 1 * (2 * produkt [3])
= 1 * (2 * (3 * produkt []))
= 1 * (2 * (3 * 1))
```

Dolžino seznama definiramo kot:

```{code-cell}
let rec dolzina xs =
  match xs with
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```

In izračunamo kot:

```ocaml
dolzina [1; 2; 3]
= 1 + dolzina [2; 3]
= 1 + (1 + dolzina [3])
= 1 + (1 + (1 + dolzina []))
= 1 + (1 + (1 + 0))
```

Slikanje elementov seznama s funkcijo `f` definiramo kot

```{code-cell}
let rec preslikaj f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: preslikaj f xs
```

in izračunamo kot:

```ocaml
preslikaj f [1; 2; 3]
= f 1 : preslikaj f [2; 3]
= f 1 : (f 2 : preslikaj f [3])
= f 1 : (f 2 : (f 3 : preslikaj f []))
= f 1 : (f 2 : (f 3 : []))
```

Splošni vzorec zajame funkcija `zlozi_desno` (_`fold_right`_), ki jo definiramo kot:

```{code-cell}
let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)
```

in izračunamo kot:

```ocaml
zlozi_desno f [1; 2; 3] z
= f 1 (zlozi_desno f [2; 3] z)
= f 1 (f 2 (zlozi_desno f [3] z))
= f 1 (f 2 (f 3 (zlozi_desno f [] z)))
= f 1 (f 2 (f 3 z))
```

Funkcija sprejme tri argumente:

- seznam `xs`,
- vrednost `z`, ki jo funkcija vrne za prazen seznam, ter
- funkcijo `f`, s katero glavo seznama zloži s sliko repa.

Na primer, vsota praznega seznama je $0$, vsota sestavljenega pa je seštevek glave in vsote repa.

```{code-cell}
zlozi_desno (+) [1; 2; 3; 4; 5] 0
```

Za vajo lahko na enak način izrazite še ostale funkcije `produkt`, `dolzina` in `preslikaj`. Funkcija `zlozi_desno` je seveda zelo uporabna, zato je na voljo tudi v standardni knjižnici in sicer pod imenom `List.fold_right`.

```{code-cell}
List.fold_right
```

Ni težko uganiti, da obstaja tudi funkcija, ki elemente zlaga v drugem vrstnem redu:

```ocaml
zlozi_levo f z [1; 2; 3]
= zlozi_levo f (f z 1) [2; 3]
= zlozi_levo f (f (f z 1) 2) [3]
= zlozi_levo f (f (f (f z 1) 2) 3) []
= (f (f (f z 1) 2) 3)
```

Definiramo jo kot:

```{code-cell}
let rec zlozi_levo f z xs =
  match xs with
  | [] -> z
  | x :: xs -> zlozi_levo f (f z x) xs
```

na voljo pa je tudi kot `List.fold_left`. Za razliko od `List.fold_right` je tudi repno rekurzivna.

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
