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

# Definicije tipov

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

## Okrajšave tipov

```{code-cell}
type 'a zaporedje = int -> 'a

type ('k, 'v) slovar = ('k * 'v) list
```

Če tip sprejme več parametrov (na primer slovar ima tako tip ključev kot tip vrednosti), jih lahko naštejemo v oklepajih.

## Zapisni tipi

```{code-cell}
type kompleksno = float * float
```

Kako bi izračunali absolutno vrednost kompleksnega števila? Ena možnost je:

```{code-cell}
let abs (x, y) = sqrt (x ** 2. +. y ** 2.)
```

Toda če smo v mislih imeli polarni zapis, je pravilna definicija:

```{code-cell}
let abs (r, _fi) = r
```


```{code-cell}
type kartezicno = {re : float; im : float}
type polarno = {radij : float; kot : float}
```


```{code-cell}
let i = {re = 0.0; im = 1.0}
```

```{code-cell}
let abs z = sqrt (z.re ** 2. +. z.im ** 2.)
```

```{code-cell}
let abs' z = z.radij
```

```{code-cell}
let konjugiraj z = {z with im = -. z.im}
```

```{code-cell}
konjugiraj i
```

Kljub temu, da zapise pišemo podobno kot Pythonove slovarje, gre za popolnoma različni strukturi. Zapisi so v resnici kartezični produkti, le da so komponente poimenovane, imena polj pa niso vrednosti, ki bi si jih lahko podajali naokoli.

## Vsote

 

```{code-cell}
type velikost = XS | S | M | L | XL
```

Tedaj bo tip imel natanko pet možnih vrednosti in OCaml nas bo opozoril, če poskusimo uporabiti nenavedeno varianto:

```{code-cell}
[XS; XS; M; S; XL; L; L]
```

```{code-cell}
:tags: [raises-exception]
[XS; XS; L; M; M; XM]
```

```{code-cell}
type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of float * float
```

```{code-cell}
[Tocka; Pravokotnik (1., 2.); Tocka; Krog 3.]
```


```{code-cell}
let povrsina obj =
  match obj with
  | Tocka -> 0.
  | Krog r -> 3.14 *. r ** 2.
  | Pravokotnik (v, s) -> v *. s
```

```{code-cell}
let obseg =
  function
  | Tocka -> 0.
  | Krog r -> 2. *. 3.14 *. r
  | Pravokotnik (v, s) -> 2. *. (v +. s)
```

## Tip `option`


```{code-cell}
let slaba_glava (x :: _) = x
```

```{code-cell}
slaba_glava [1; 2; 3]
```

```{code-cell}
:tags: [raises-exception]
slaba_glava []
```


```ocaml
type 'a option = None | Some of 'a
```


Takim tipom pravimo tudi _induktivni_ ali _algebrajski_ tipi. Najenostavnejši primer induktivnega tipa so naravna števila. Predstavimo jih z vsoto z dvema konstruktorjema `Nic` in `Naslednik`, pri čemer slednji sprejme en argument, ki je zopet naravno število.

```{code-cell}
type naravno = Nic | Naslednik of naravno
```

Vsoto naravnih števil podamo z običajno rekurzivno definicijo:

```{code-cell}
let rec vsota m n =
  match m with
  | Nic -> n
  | Naslednik m' -> Naslednik (vsota m' n)
```

Induktivne tipe se pogosto uporablja za predstavitev izrazov določenega formalnega jezika. Na primer, aritmetične izraze gradimo iz števil ter aritmetičnih operacij. Take izraze bi lahko predstavili s tipom:

```{code-cell}
type izraz =
  | Stevilo of int
  | Plus of izraz * izraz
  | Minus of izraz
  | Krat of izraz * izraz
```

Na primer, izrazu $-(5 \times (2 + 7))$ bi ustrezala vrednost

```{code-cell}
let i = Minus (
  Krat (Stevilo 5, Plus (Stevilo 2, Stevilo 7))
)
```

Za vajo lahko napišete rekurzivno funkcijo `izracunaj : izraz -> int`, ki dani izraz prevori v njegovo vrednost. Na primer, za zgornji izraz bi funkcija vrnila `-45`.

Še en induktivni tip, ki ga bomo podrobneje spoznali v kratkem, pa so dvojiška drevesa. Dvojiško drevo je bodisi prazno bodisi ima koren, v katerem je shranjena vrednost, ter dva otroka, ki sta zopet drevesi, na primer (pri čemer praznih dreves ne kažemo):

![](slike/09-iskalna-drevesa/avl-drevo.png)

Tip dvojiških dreves podamo s tipom

```{code-cell}
type 'a drevo =
  | Prazno
  | Sestavljeno of 'a drevo * 'a * 'a drevo
```
