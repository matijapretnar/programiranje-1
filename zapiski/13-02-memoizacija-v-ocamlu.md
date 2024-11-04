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

# Memoizacija v OCamlu

Osnovna memoizacija v OCamlu poteka podobno tisti v Pythonu. Za začetek si spet poglejmo funkcijo, ki vrne kvadrat celega števila:

```{code-cell}
let kvadrat x =
    print_endline ("Računam " ^ string_of_int x);
    x * x
```

```{code-cell}
kvadrat 10
```

```{code-cell}
kvadrat 10
```

S pomočjo te funkcije lahko definiramo funkcijo `mem_kvadrat`, ki si shranjuje že izračunane vrednosti. Za shranjevanje uporabimo knjižnico `Hashtbl` za delo z zgoščevalnimi tabelami, s katerimi so implementirani tudi Pythonovi slovarji.

```{code-cell}
let kvadrati = Hashtbl.create 512 (* argument 512 predstavlja pričakovano začetno velikost tabele *)
let mem_kvadrat x =
  match Hashtbl.find_opt kvadrati x with
  | Some y -> y
  | None ->
      let y = kvadrat x in
      Hashtbl.add kvadrati x y;
      y
```

```{code-cell}
mem_kvadrat 10
```

```{code-cell}
mem_kvadrat 10
```

Tip tabele `kvadrati` lahko ignorirate, sporoča pa, da sta tipa ključev in vrednosti zaenkrat še neznana, vendar nista polimorfna. V resnici že definicija funkcije `mem_kvadrat` povzroči, da se oba nastavita na `int`.

```{code-cell}
kvadrati
```

Tudi v OCamlu lahko napišemo funkcijo višjega reda, ki memoizira dano funkcijo:

```{code-cell}
let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f
```

```{code-cell}
let mem_kvadrat2 = memoiziraj kvadrat
```

```{code-cell}
mem_kvadrat2 10
```

```{code-cell}
mem_kvadrat2 10
```

## Memoizacija rekurzivnih funkcij

Pri memoizaciji rekurzivnih funkcij pa nastopijo težave.

```{code-cell}
let rec fib n =
  print_endline ("Računam " ^ string_of_int n);
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)

let mem_fib = memoiziraj fib
```

```{code-cell}
mem_fib 4
```

```{code-cell}
mem_fib 4
```

Na prvi pogled je videti, kot da memoizacija deluje pravilno, saj je drugi klic `mem_fib` vrnil že izračunano vrednost. Vendar ob natančnem pregledu vidimo, da se ja primer vrednost pri `2` izračunala večkrat. Težava je v tem, da si `mem_fib` shrani vrednosti, na katerih je bil poklican. Če pa rezultata še ne pozna, pokliče funkcijo `fib`, ki pa o že izračunanih vrednostih ne ve nič, kar vodi do velikega števila klicev. Če `mem_fib` na primer pokličemo na 5, mu poprej izračunana vrednost nič ne pomaga, saj `fib 5` pokliče `fib 4` in ne `mem_fib 4`.

```{code-cell}
mem_fib 5
```

Tudi če si `mem_fib` shranimo pod isto ime, ne rešimo ničesar.

```{code-cell}
let fib = memoiziraj fib
```

```{code-cell}
fib 5
```

Kljub istemu imenu gre za dve različni funkciji: eno, ki smo jo zgoraj definirali rekurzivno, in drugo, ki je bila rezultat klica `memoiziraj`. V Pythonu ta težava ne nastopi, saj je _dinamičen jezik_. To pomeni, da računalnik ob klicu funkcije ne skoči na vnaprej (_statično_) določeno mesto v programski kodi, temveč šele takrat pogleda, kaj se skriva pod tem imenom. V našem primeru lahko to izkoristimo, da pod to ime shranimo drugo funkcijo. Seveda so dinamični jeziki zaradi te fleksibilnosti počasnejši in tudi manj varni.

Še vedno pa si želimo splošnega načina za memoizacijo rekurzivnih funkcij. Kot smo videli, je težava v tem, da rekurzivne funkcije kličejo same sebe, mi pa se želimo v te klice vriniti. To dosežemo tako, da funkciji podamo dodaten argument, s katerim povemo, katero funkcijo naj pokliče namesto sebe. Na primer, rekurzivni definiciji

```{code-cell}
let rec fib n =
  print_endline ("Računam " ^ string_of_int n);
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)
```

kot smo jo videli prej, kot dodaten argument `f` podamo funkcijo, ki naj jo pokliče namesto sebe (opazimo, da v tem primeru funkcija ni več rekurzivna, zato ključna beseda `rec` ni potrebna). Takim funkcijam pravimo, da so odvite (_unrolled_), saj smo rekurzivno zanko prekinili.

```{code-cell}
let odviti_fib f n =
  print_endline ("Računam " ^ string_of_int n);
  match n with
  | 0 | 1 -> n
  | n -> f (n - 1) + f (n - 2)
```

Za `f` lahko podamo poljubno funkcijo, na primer tako, ki vedno vrača 42:

```{code-cell}
let nagajivi_fib n = odviti_fib (fun _ -> 42) n
```

```{code-cell}
nagajivi_fib 10
```

```{code-cell}
nagajivi_fib 200
```

Če za `f` podamo dobljeno funkcijo, dobimo ravno prvotno rekurzivno definicijo:

```{code-cell}
let rec fib n = odviti_fib fib n
```

```{code-cell}
fib 5
```

Seveda pa je naš namen, da v klic vrinemo funkcijo, ki hrani rezultate:

```{code-cell}
let rezultati = Hashtbl.create 512
let rec mem_fib x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_fib mem_fib x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
```

```{code-cell}
mem_fib 5
```

```{code-cell}
mem_fib 6
```

Vidimo, da se je vsaka vrednost izračunala natanko enkrat. Postopek sedaj lahko naredimo tudi v splošnem:

```{code-cell}
let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = odviti_f mem_f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f
```

```{code-cell}
let mem_fib = memoiziraj_rec odviti_fib
```

```{code-cell}
mem_fib 5
```

```{code-cell}
mem_fib 6
```

Z ustreznim poimenovanjem lahko pridemo do oblike, ki je z izjemo prve vrstice (in zamika in oklepaja) enaka naši prvotni naivni rekurzivni definiciji:

```{code-cell}
let fib = memoiziraj_rec (fun fib n ->
  print_endline ("Računam " ^ string_of_int n);
  match n with
  | 0 | 1 -> n
  | n -> fib (n - 1) + fib (n - 2)
)
```

```{code-cell}
fib 5
```

```{code-cell}
fib 6
```
