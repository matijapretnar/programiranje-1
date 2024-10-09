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

# Učinki

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

## Učinki v OCamlu

Za začetek si poglejmo nekaj primerov učinkov v OCamlu. Prva (in zelo pogosto uporabljana) je `print_endline`, ki sprejme niz ter ga izpiše na zaslon in vrne prazen nabor. V splošnem nam prisotnost tipa `unit` v OCamlu sporoča, da se bodo sprožili stranski učinki, saj sicer funkcija, ki bi vrnila `unit`, ne bi storila ničesar koristnega.

```{code-cell}
print_endline
```

```{code-cell}
print_endline "Hello, world!"
```

Pri klicu funkcij z učinki je včasih uporabna tudi funkcija `List.iter`, ki dano funkcijo pokliče na vseh elementih seznama. Podobna je funkciji `List.map`, le da seznam vrnjenih praznih naborov zavrže, saj ga redkokdaj uporabljamo.

```{code-cell}
List.map print_endline ["am"; "bam"; "pet"; "podgan"]
```

```{code-cell}
List.iter print_endline ["am"; "bam"; "pet"; "podgan"]
```

Za delo z psevdonaključnimi vrednostmi je na voljo knjižnica `Random`. Pogosto uporabljani funkciji tam sta `Random.bool`, ki ob vsakem klicu vrne naključno logično vrednost, in `Random.int`, ki za neko število $m$ vrne naključno celo število med 0 in $m - 1$. Pri uporabi psevdonaključnih števil moramo paziti, da na začetku nastavimo ustrezno seme, saj sicer OCaml vedno začne z enakim. Na primer, spodnji klic bo ob prvem klicu vedno vrnil število 9344.

```{code-cell}
Random.int 100000
```

Seme lahko najbolj enostavno nastavimo z `Random.self_init`, ki ga nastavi glede na trenuten čas, stanje procesov, …, skratka dovolj naključno.

```{code-cell}
Random.self_init ()
```

```{code-cell}
Random.int 100000 (* tu je nemogoče napovedati, kaj bo klic vrnil *)
```

Za branje s konzole uporabimo funkcijo `read_line`.

```{code-cell}
read_line
```


```{code-cell}
raise
```
