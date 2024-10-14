# Končni avtomati

Projekt vsebuje implementacijo končnih avtomatov, enega najpreprostejših računskih modelov, ter njihovo uporabo pri karakterizaciji nizov. Končni avtomat začne v enem izmed možnih stanj, nato pa glede na trenutno stanje in trenutni simbol preide v neko novo stanje. Če ob pregledu celotnega niza konča v enem od sprejemnih stanj, je niz sprejet, sicer pa ni.

Za tekoči primer si oglejmo avtomat, ki sprejema nize, sestavljene iz ničel in enic, v katerih da vsota enic pri deljenju s 3 ostanek 1. Tak avtomat predstavimo z naslednjim diagramom, na katerem je začetno stanje označeno s puščico, sprejemna stanja pa so dvojno obkrožena.

TODO

## Matematična definicija

Končni avtomat je definiran kot nabor $(\Sigma, Q, q_0, F, \delta)$, kjer so:

- $\Sigma$ množica simbolov oz. abeceda,
- $Q$ množica stanj,
- $q_0 \in Q$ začetno stanje,
- $F \subseteq Q$ množica sprejemnih stanj in
- $\delta : Q \times \Sigma \to Q$ prehodna funkcija.

Na primer, zgornji končni avtomat predstavimo z naborom $(\{0, 1\}, \{q_0, q_1, q_2\}, q_0, \{q_1\}, \delta)$, kjer je $\delta$ podana z naslednjo tabelo:

| $\delta$ | `0`   | `1`   |
| -------- | ----- | ----- |
| $q_0$    | $q_0$ | $q_1$ |
| $q_1$    | $q_2$ | $q_0$ |
| $q_2$    | $q_1$ | $q_2$ |

## Navodila za uporabo

Ker projekt služi kot osnova za večje projekte, so njegove lastnosti zelo okrnjene. Konkretno implementacija omogoča samo zgoraj omenjeni končni avtomat. Na voljo sta dva vmesnika, tekstovni in grafični. Oba prevedemo z ukazom `dune build`, ki v korenskem imeniku ustvari datoteko `tekstovniVmesnik.exe`, v imeniku `html` pa JavaScript datoteko `spletniVmesnik.bc.js`, ki se izvede, ko v brskalniku odpremo `spletniVmesnik.html`.

Če OCamla nimate nameščenega, lahko še vedno preizkusite tekstovni vmesnik prek ene od spletnih implementacij OCamla, najbolje <http://ocaml.besson.link/>, ki podpira branje s konzole. V tem primeru si na vrh datoteke `tekstovniVmesnik.ml` dodajte še vrstice

```ocaml
module Avtomat = struct
    (* celotna vsebina datoteke avtomat.ml *)
end
```

### Tekstovni vmesnik

TODO

### Spletni vmesnik

TODO

## Implementacija

### Struktura datotek

TODO

### `avtomat.ml`

TODO

### `model.ml`

TODO
