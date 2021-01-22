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

Poleg bogatega nabora vgrajenih tipov si tipe v OCamlu lahko definiramo tudi sami.

## Okrajšave tipov

Najenostavnejši način za definicijo tipov so okrajšave obstoječih tipov. Na primer, tip za $\mathbb{R}^3$ si lahko definiramo kot:

```{code-cell}
type r3 = float * float * float
```

Tako kot na primer vgrajeni tip `list` lahko tudi naši tipi vsebujejo parametre:

```{code-cell}
type 'a zaporedje = int -> 'a

type ('k, 'v) slovar = ('k * 'v) list
```

Če tip sprejme več parametrov (na primer slovar ima tako tip ključev kot tip vrednosti), jih lahko naštejemo v oklepajih.

## Zapisni tipi

Recimo, da si definiramo kompleksna števila s pari realnih:

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

Zmešnjavi se lahko izognemo, če obe komponenti poimenujemo. V OCamlu to storimo z zapisnimi tipi, ki jih podamo tako, da naštejemo imena polj ter njihove tipe:

```{code-cell}
type kartezicno = {re : float; im : float}
type polarno = {radij : float; kot : float}
```

Vrednosti tipov pišemo podobno, le da jih podamo z `=`:

```{code-cell}
let i = {re = 0.0; im = 1.0}
```

Do komponent dostopamo z `zapis.ime_polja`:

```{code-cell}
let abs z = sqrt (z.re ** 2. +. z.im ** 2.)
```

```{code-cell}
let abs' z = z.radij
```

Kljub temu, da zapise pišemo podobno kot Pythonove slovarje, gre za popolnoma različni strukturi. Zapisi so v resnici kartezični produkti, le da so komponente poimenovane, imena polj pa niso vrednosti, ki bi si jih lahko podajali naokoli.

## Vsote

Najzanimivejši tip, ki ga lahko definiramo, pa so _vsote_. Podamo jih tako, da naštejemo možne variante, od katerih je vsaka podana s svojim _konstruktorjem_. Če se želimo omejiti na fiksno množico velikosti oblačil, lahko na primer napišemo enostavno vsoto s petimi variantami:

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

Vsaka izmed naštetih konstruktorjev lahko sprejme tudi argumente vnaprej določenega tipa:

```{code-cell}
type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of float * float
```

```{code-cell}
[Tocka; Pravokotnik (1., 2.); Tocka; Krog 3.]
```

Tako kot vsote naštejemo po kosih, lahko prek `match` ali `function` po kosih tudi definiramo funkcije na njih.

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

Včasih imamo opravka s funkcijami, ki jih ne moremo povsod dobro definirati. Na primer, glava seznama je definirana samo za sestavljeni seznam. Če želimo, lahko naštejemo le to možnost, vendar bomo potem dobili napako ob izvajanju.

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

Varnejši način je, da uporabimo tip `'a option`, ki predstavlja morebitno vrednost tipa `'a`:

```ocaml
type 'a option = None | Some of 'a
```

Nato bi lahko glavo seznama definirali tako, da vedno vrne vrednost:

```{code-cell}
let glava = function
  | [] -> None
  | x :: _ -> Some x
```

```{code-cell}
glava [1; 2; 3]
```

```{code-cell}
glava []
```

Če uporabimo tip `option`, nas tipi prisilijo, da obravnavamo robne primere. Na primer, prej bi lahko napisali:

```{code-cell}
let ali_je_slaba_glava_velika xs = slaba_glava xs > 100
```

Če uporabimo varnejši način, pa rezultata ne moremo neposredno primerjati s `100`:

```{code-cell}
:tags: [raises-exception]
let ali_je_glava_velika xs = glava xs > 100
```

Tipi nas prisilijo, da obravnavamo vse primere in se odločimo, kaj naredimo, če podatka ni na voljo. Lahko bi se na primer odločili, da seznam brez glave nima velike glave:

```{code-cell}
let ali_je_glava_velika xs =
  match glava xs with
  | None -> false
  | Some x -> x > 100
```

Lahko bi se odločili tudi drugače, na primer da spet vrnemo morebitni odgovor, v vsakem primeru pa ne bomo pozabili na noben primer:

```{code-cell}
let ali_je_glava_velika xs =
  match glava xs with
  | None -> None
  | Some x -> Some (x > 100)
```

Vsote so lahko definirane tudi rekurzivno. Takim tipom pravimo tudi _induktivni_ ali _algebrajski_ tipi. Najenostavnejši primer induktivnega tipa so naravna števila. Predstavimo jih z vsoto z dvema konstruktorjema `Nic` in `Naslednik`, pri čemer slednji sprejme en argument, ki je zopet naravno število.

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

Še en že poznan primer induktivnega tipa so seznami. Vsak seznam je bodisi prazen, bodisi sestavljen iz glave in repa:

```{code-cell}
type 'a list =
  | Prazen
  | Sestavljen of 'a * 'a list
```

Sedaj tudi vidimo, zakaj `::` lahko uporabljamo v vzorcih - ni namreč običajna funkcija za sestavljanje seznamov, temveč konstruktor tipa seznamov.

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
