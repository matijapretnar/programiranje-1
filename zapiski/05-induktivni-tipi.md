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

# Učinki in čistost

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Računalniške funkcije so podobne matematičnim v tem, da iz vhodnih vrednosti izračunajo izhodno, vendar se tu podobnost neha. Računalniške funkcije dajo lahko ob enakih vhodih tudi različne izhodne vrednosti, poleg izračuna vrednosti pa včasih še kaj izpišejo na zaslon, preberejo s tipkovnice, sprožijo izjeme, spreminjajo datoteke, pošiljajo podatke in podobno. Vsem odklonom od čistih matematični vrednosti pravimo _stranski_ ali _računski učinki_.

Učinke si bomo ogledali naslednjič, tokrat pa si oglejmo funkcije brez njih, tako imenovane _čiste_ funkcije. Kljub temu, da z njimi lahko postorimo manj, imajo prednost, da o njih lažje razmišljamo. Zaradi tega je v splošnem dobro, da v naših programih napišemo čimveč čistih funkcij, funkcije z učinki pa omejimo kolikor se da. Tako bodo naši programi jasnejši in z manj napakami.

Poleg tega pa si lahko ob čistih funkcijah privoščimo čisto prave matematične dokaze. Da bomo imeli kaj, s čimer bomo lahko delali, si definirajmo sledeče tri funkcije:

```{code-cell}
let rec (@) xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: (xs' @ ys)
```

```{code-cell}
let rec obrni = function
  | [] -> []
  | x :: xs -> obrni xs @ [x]
```

```{code-cell}
let rec dolzina = function
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```

Iz definicij takoj sledijo enačbe

1. `[] @ ys = ys`
2. `(x :: xs) @ ys = x :: (xs @ ys)`
3. `obrni [] = []`
4. `obrni (x :: xs) = obrni xs @ [x]`
5. `dolzina [] = 0`
6. `dolzina (x :: xs) = 1 + dolzina xs`

Iz teh enačb na primer takoj sledi trditev `obrni [x] = [x]`.

    obrni [x]
    = obrni (x :: [])
        (ker je `[x]` okrajšava za `x :: []`)
    = [] @ [x]
        (po (4))
    = [x]
        (po (1))

## Indukcija na seznamih

### Trditev: `dolzina (xs @ ys) = dolzina xs + dolzina ys`

Tukaj enostavno odvijanje definicij ne bo pomagalo, saj izraz `dolzina (xs @ ys)` ne ustreza ne levi ne desni strani nobene od zgoraj naštetih definicij. Namesto tega uporabimo načelo indukcije za sezname:

    P([]) ∧ (∀ z, zs. P(zs) ⇒ P(z :: zs)) ⟹ ∀ ws. P(ws)

Torej, lastnost `P` velja za vse sezname `ws`, kadar (1) velja za prazen seznam `[]` in (2) velja za sestavljen seznam `z :: zs` ob predpostavki, da velja za rep `zs`. Načelo indukcije je podobno načelu indukcije za naravna števila:

    P(0) ∧ (∀ m. P(m) ⇒ P(m⁺)) ⟹ ∀ n. P(n)

Torej, lastnost `P` velja za vsa naravna števila `n`, kadar (1) velja za `0` in (2) velja za naslednika `m⁺` ob predpostavki, da velja za `m`. Izjavo dokažemo z indukcijo na levi seznam. Indukcija poteka v dveh korakih:

#### Osnovni korak

V osnovnem koraku pokažemo, da enakost velja, kadar je levi seznam prazen, torej oblike `[]`:

    dolzina ([] @ ys)
    = dolzina ys
        (po (1))
    = 0 + dolzina ys
        (po Peanovih aksiomih)
    = dolzina [] + dolzina ys
        (po (5))

#### Indukcijski korak

V indukcijskem koraku pokažemo, da enakost velja za sestavljeni seznam `x :: xs` ob predpostavki, da enakost velja za seznam `xs`:

    dolzina ((x :: xs) @ ys)
    = dolzina (x :: (xs @ ys))
        (po (2))
    = 1 + dolzina (xs @ ys)
        (po (6))
    = 1 + (dolzina xs + dolzina ys)
        (po indukcijski prepostavki)
    = (1 + dolzina xs) + dolzina ys)
        (po Peanovih aksiomih)
    = dolzina (x :: xs) + dolzina ys
        (po (6))

### Pravilnost načela indukcije na seznamih

Zakaj sploh smemo uporabiti načelo indukcije? Recimo, da velja

    P([]) ∧ (∀ z, zs. P(zs) ⇒ P(z :: zs))

Zakaj potem velja `P(ws)` za poljuben seznam `ws`? Vzemimo nek konkreten seznam, na primer `[1; 2; 3]`, in pokažimo, da velja `P([1; 2; 3])`. Res, po prvem delu predpostavke velja `P([])`. Zato po drugem delu predpostavke velja `P([3])`, saj velja `P([])` in je `[3] = 3 :: []`. Podobno potem iz `P([3])` sledi `P([2; 3])` in na koncu tudi `P([1; 2; 3])`. Tak dokaz bi lahko ponovili za vsak konkreten seznam.

V splošnem pa se naslonimo na načelo indukcije za naravna števila. Definirajmo:

    Q(n) :=  ∀ ws. |ws| = n ⇒ P(ws)

Torej, `Q(n)` velja, če velja `P(ws)` za vse sezname `ws` dolžine `n`. Iz Peanovih aksiomov potem sledi:

    Q(0) ∧ (∀m. Q(m) ⇒ Q(m+)) ⟹ ∀n. Q(n)

Pokažimo, da velja `Q(0)` in `∀m. Q(m) ⇒ Q(m+)`. Ker velja `P([])` in je `[]` edini seznam dolžine `0`, velja tudi `Q(0)`. Vzemimo poljuben `m` in predpostavimo `Q(m)`. Torej za poljuben seznam `zs` dolžine `m` velja `P(zs)`. Iz predpostavke potem sledi, da velja tudi `P(z :: zs)`. Ker so vsi seznami dolžine `m⁺` take oblike, smo pokazali `Q(m⁺)`. Po indukciji na naravnih številih torej sledi `∀n. Q(n)`. Ker je vsak seznam `ws` končen, ima za dolžino neko naravno število, zato velja `P(ws)` za vse sezname `ws`.

### Trditev: `xs @ [] = xs`

Po (1) vemo, da je `[] @ xs = xs`, torej je `[]` leva enota za `@`. To, da je `[]` tudi desna enota, pa ni samoumevno --- za dokaz uporabimo indukcijo.

#### Osnovni korak

Po (1) velja `[] @ [] = []`, kar dokaže osnovni korak.

#### Indukcijski korak

Prepostavimo, da velja `xs @ [] = xs`. Tedaj velja

    (x :: xs) @ []
    = x :: (xs @ [])
        (po (2))
    = x :: xs
        (po indukcijski prepostavki)

kar zaključi tudi indukcijski korak.


### Trditev `xs @ (ys @ zs) = (xs @ ys) @ zs`

Operacija stikanja seznamov `@` je tudi asociativna, kar dokažemo z indukcijo na `xs`. Če zapišete dokaz asociativnosti seštevanja, lahko vidite, da poteka podobno, le da se namesto `[]` pojavlja `0`, namesto `x :: xs` pa naslednik `n⁺`.

#### Osnovni korak

    [] @ (ys @ zs)
    = ys @ zs
        (po (1))
    = ([] @ ys) @ zs
        (po (1))

#### Indukcijski korak

Prepostavimo, da velja `xs @ (ys @ zs) = (xs @ ys) @ zs`. Tedaj velja

    (x :: xs) @ (ys @ zs)
    = x :: (xs @ (ys @ zs))
        (po (2))
    = x :: ((xs @ ys) @ zs)
        (po indukcijski predpostavki)
    = (x :: (xs @ ys)) @ zs
        (po (2))
    = ((x :: xs) @ ys) @ zs
        (po (2))

### Trditev `obrni (xs @ ys) = obrni ys @ obrni xs`

#### Osnovni korak

    obrni ([] @ ys)
    = obrni ys
        (po (1))
    = obrni ys @ []
        (po prej dokazani trditvi ∀ xs. xs @ [] = xs)
    = obrni ys @ obrni []
        (po (3))

#### Indukcijski korak

    obrni ((x :: xs) @ ys)
    = obrni (x :: (xs @ ys))
        (po (2))
    = obrni (xs @ ys) @ [x]
        (po (4))
    = (obrni ys @ obrni xs) @ [x]
        (po indukcijski predpostavki)
    = obrni ys @ (obrni xs @ [x])
        (po prej dokazani trditvi ∀ xs, ys, zs. xs @ (ys @ zs) = (xs @ ys) @ zs)
    = obrni ys @ obrni (x :: xs)
        (po (4))

### Trditev `dolzina (obrni xs) = dolzina xs`

#### Osnovni korak

Iz definicije (3) sledi `dolzina (obrni []) = dolzina []`,

#### Indukcijski korak

Za indukcijski korak moramo pokazati, da velja `dolzina (obrni (x :: xs)) = dolzina (x :: xs)` ob indukcijski predpostavki `dolzina (obrni xs) = dolzina xs`.
Velja:

    dolzina (obrni (x :: xs))
    = dolzina (obrni xs @ [x])
        (po definiciji (4))
    = dolzina (obrni xs) + dolzina [x]
        (po zgornji lemi)
    = dolzina xs + dolzina [x]
        (po indukcijski predpostavki)
    = dolzina xs + 1
        (po definicijah (5) in (6))
    = 1 + dolzina xs
        (zaradi komutativnosti seštevanja)
    = dolzina (x :: xs)
        (po definiciji (6))

s čimer zaključimo tudi indukcijski korak.

## Indukcija na drevesih

Načela indukcije imamo na voljo tudi na drugih vsotah.
Na primer, če definiramo tip dvojiških dreves:

```ocaml
type 'a drevo =
  | Prazno
  | Sest of 'a drevo * 'a * 'a drevo
```

zanj velja načelo indukcije:

    P(Prazno) ∧ (∀ l, x, d. P(l) ∧ P(d) ⇒ P(Sest (l, x, d))) ⟹ ∀ t. P(t)

Torej, lastnost `P` velja za vsa drevesa `t`, kadar (1) velja za prazno drevo `Prazno` in (2) velja za sestavljeno drevo `Sest (l, x, d)` ob predpostavki, da velja za otroka `l` in `d`.

Definirajmo funkciji:

```ocaml
let rec zrcali = function
  | Prazno -> Prazno
  | Sest (l, x, d) -> Sest (zrcali d, x, zrcali l)

let rec globina = function
  | Prazno -> 0
  | Sest (l, _, d) -> 1 + max (globina l) (globina d)
```

Iz definicij sledijo enačbe

1. `zrcali Prazno = Prazno`
2. `zrcali (Sest (l, x, d)) = Sest (zrcali d, x, zrcali l)`
3. `globina Prazno = 0`
4. `globina (Sest (l, x, d)) = 1 + max (globina l) (globina d)`


### Trditev `globina (zrcali t) = globina t`

#### Osnovni korak

    globina (zrcali Prazno)
    = globina Prazno
        (po (1))

#### Indukcijski korak

    globina (zrcali (Sest (l, x, d)))
    = globina (Sest (zrcali d, x, zrcali l))
        (po (2))
    = 1 + max (globina (zrcali d)) (globina (zrcali l))
        (po (4))
    = 1 + max (globina d) (globina (zrcali l))
        (po prvi indukcijski predpostavki)
    = 1 + max (globina d) (globina l)
        (po drugi indukcijski predpostavki)
    = 1 + max (globina l) (globina d)
        (zaradi komutativnosti max)
    = globina (Sest (l, x, d))
        (po (4))

## Vaje za utrjevanje

### Trditev `obrni (obrni xs) = xs`

### Trditev `zrcali (zrcali t) = t`

### Trditev `obrni xs = obrni' xs` (težja)

Kot vemo, ima funkcija `obrni`, definirana kot:

```ocaml
let rec obrni = function
  | [] -> []
  | x :: xs -> obrni xs @ [x]
```

časovno zahtevnost $O(n^2)$, saj se mora zapeljati čez ves seznam, da mu na konec doda `x`.

Bolje je, če uporabimo funkcijo `obrni'`, ki uporablja akumulator in je definirana kot:

```ocaml
let obrni' =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux []
```

Pokažite, da za vse sezname `xs` velja `obrni xs = obrni' xs`.

### Indukcija na drugih tipih

Definirajmo tipe

```ocaml
type naravno = Zero | Succ of naravno
type boole = Resnica | Laz
type 'a mogoce = Nic | Nekaj of 'a
```

Kakšni sta videti načeli indukcije za te tipe?
