# Domača naloga

Domača naloga od vas zahteva da napišete program, ki reši uganko [sudoku](https://sl.wikipedia.org/wiki/Sudoku). Osnovno ogrodje smo vam že pripravili, samo pa morate dokončati nekaj manjkajočih delov in izboljšati učinkovitost reševanja. Za več točk se od vas pričakuje, da boste nekatere že pripravljene funkcije popravili in dodali zmožnost reševanja nestandardnih sudokujev. Poleg pravilnega in učinkovitega delovanja se pričakuje, da bo programska koda berljiva. To ne pomeni samo tega, da so spremenljivke ustrezno poimenovane, koda lepo zamaknjena, zahtevnejši deli pa pokomentirani, temveč tudi to, da je program napišete v duhu funkcijskega programiranja.

**NALOGO MORATE REŠEVATI SAMOSTOJNO. ČE NE VESTE, ALI DOLOČENA STVAR POMENI SODELOVANJE, RAJE VPRAŠAJTE!**

## Terminologija

Pri sudokuju uporabljamo sledečo terminologijo:

- **grid**: Celotna mreža velikosti 9×9, ki je lahko delno ali v celoti izpolnjena.
- **row**: Ena izmed devetih vrstic mreže. Vrstice so indeksirane od 0 do 8 kot kaže spodnji primer:

      ┏━━━┯━━━┯━━━┓
      ┃000│000│000┃
      ┃111│111│111┃
      ┃222│222│222┃
      ┠───┼───┼───┨
      ┃333│333│333┃
      ┃444│444│444┃
      ┃555│555│555┃
      ┠───┼───┼───┨
      ┃666│666│666┃
      ┃777│777│777┃
      ┃888│888│888┃
      ┗━━━┷━━━┷━━━┛

- **column**: Eden izmed devetih stolpcev mreže. Stolpci so indeksirani od 0 do 8 kot kaže spodnji primer:

      ┏━━━┯━━━┯━━━┓
      ┃012│345│678┃
      ┃012│345│678┃
      ┃012│345│678┃
      ┠───┼───┼───┨
      ┃012│345│678┃
      ┃012│345│678┃
      ┃012│345│678┃
      ┠───┼───┼───┨
      ┃012│345│678┃
      ┃012│345│678┃
      ┃012│345│678┃
      ┗━━━┷━━━┷━━━┛

- **box**: Ena izmed devetih škatel velikost 3×3. Škatle so indeksirane od 0 do 8 kot kaže spodnji primer:

      ┏━━━┯━━━┯━━━┓
      ┃000│111│222┃
      ┃000│111│222┃
      ┃000│111│222┃
      ┠───┼───┼───┨
      ┃333│444│555┃
      ┃333│444│555┃
      ┃333│444│555┃
      ┠───┼───┼───┨
      ┃666│777│888┃
      ┃666│777│888┃
      ┃666│777│888┃
      ┗━━━┷━━━┷━━━┛

- **cell**: Ena izmed 81 celic v mreži sudokuja. Celice so običajno tipa `int option`, saj bodisi vsebujejo števko bodisi so prazne.
- **digit**: Ena izmed števk od 1 do 9. Potreben pogoj za pravilno rešitev sudokuja je to, da je v vsaki celici natanko ena števka.

## Ogrodje domače naloge

Pripravljeno ogrodje domače naloge se nahaja v treh `.ml` datotekah:

- `model.ml`: Vsebuje model, osnovne funkcije za branje in izpisovanje in preverjanje pravilnosti rešitve standardnega sudokuja.
- `solver.ml`: Vsebuje funkcije za reševanje sudokuja. Za osnovno verzijo naloge boste večinoma spreminjali in dopolnjevali to datoteko.
- `main.ml`: Prebere sudoku iz datoteke in ga s pomočjo funkcij v `solver.ml` reši.

## Uporaba

V ukazni vrstici poženite:

    ocamlc -g model.ml solver.ml main.ml -o sudoku.exe

Če ste si OCaml namestili na sistemu Windows s pomočjo `fdopen` namesto `ocamlc` uporabite:

    C:\OCaml64\usr\local\bin\ocaml-env.exe exec -- C:\OCaml64\home\???\.opam\4.12.0+mingw64c\bin\ocamlc.exe

To ustvari datoteko `sudoku.exe`, ki ob klicu

    ./sudoku.exe primer1.sdk primer2.sdk ...

reši sudokuje v datotekah `primer1.sdk`, `primer2.sdk`, ...

V mapi `sudokuji` najdete testne datoteke, ki vsebujejo primere standardnih sudokujev. Program lahko na vseh sudokujih poženete kot

    ./sudoku.exe sudokuji/*.sdk

ali

    ./sudoku.exe sudokuji/obicajni*.sdk sudokuji/puscice*.sdk

Če boste med reševanjem naloge naleteli na težavo z lokalno namestitvijo OCamla, to čim prej sporočite asistentu Filipu, da vam lahko pomagamo usposobiti vse potrebno.

Če boste domačo nalogo reševali v okoljih, ki ne podpirajo branja datotek (npr. prek spletne storitve), si poglejte spodnji del datoteke `main.ml`, kjer je prikazana uporaba v takem primeru. Za delo v takih okoljih boste verjetno morali tudi vse datoteke združiti v eno samo. To najlaže naredite tako, da jih po vrsti skopirate v eno datoteko in odstranite kvalificirane klice (npr. klic oblike `Model.problem_of_string` nadomestite s `problem_of_string`).

## Kriterij

**NALOGO MORATE REŠEVATI SAMOSTOJNO. ČE NE VESTE, ALI DOLOČENA STVAR POMENI SODELOVANJE, RAJE VPRAŠAJTE!**

Z domačo nalogo je možno doseči do 20 točk. Pri točkovanju velja sledeči kriterij:

- 5 točk : Program, ki pravilno reši vsakega od priloženih običajnih sudokujev.
- 5 točk : Berljivost in eleganca kode.
- 5 točk : Učinkovitost programa za reševanje.
- 5 točk : Razširitve programa na nestandardne sudokuje (kletke, puščice, termometri) - NATANČNA NAVODILA SLEDIJO V KRATKEM

### Razširitve

Zadnji del točk pridobite z razširitvijo programa na nestandardne sudokuje.
Nekaj primerov nestandardnih sudokujev je na voljo v mapi `sudokuji`, vendar se za višjo oceno pričakuje, da boste samo poskrbeli za testne primere, ki res preverijo učinkovitost implementacije.

Možne razširitve so:

- Termometri: Poleg običajnih pravil sudokuja, so na mreži tudi "termometri".
Števila v celicah termometra morajo v istem termometru strogo naraščati od začetka termometra do konca.
Vrstica pogoja, ki predstavlja termometer se začne z `T: `, čemur sledijo s podpičjem ločene koordinate celic na tem termometru.
Številka v prvi celici mora biti najmanjša, v drugi večja od tiste v prvi, ..., in v zadnji največja.  
- Puščice: Poleg običajnih pravil sudokuja, so na mreži tudi "puščice".
Puščica se začne na podani celici in kaže ne eno ali več različnih celic, katerih vsota mora biti enaka številu, ki je na začetni celici.
Vrstica pogoja, ki predstavlja puščico se začne z `A: `, čemur sledi opis za puščico v obliki `glava -> rep`.
Glava je celica, ki se nahaja na začetku puščice, rep pa je s podpičjem ločen seznam celic, na katere kaže glava.
Vsota števil v celicah repa more biti enaka kot število v glavi.
- Kletke: Poleg običajnih pravil sudokuja, so na mreži tudi "kletke".
Kletke so skupek povezanih celic, za katere zahtevamo, da vsebujejo sama različna števila, ki imajo podano vsoto.
Vrstica pogoja, ki predstavlja kletko se začne z `K: X `, čemur sledijo s podpičjem ločene celice, katerih vsota mora biti enaka `X`.

Vhodni podatki razširjenih testnih primerov so v datotekah `sudokuji/puscice*.sdk`, `sudokuji/termometri*.sdk`, `sudokuji/kletke*.sdk`.
Vhodni podatki se začnejo z običajno mrežo kot pri obicajnih sudokujih, čemur sledi prazna vrstica in vrstice, ki opisujejo dodatne zahteve.
Pri branju vse vrstice, ki se začnejo z `#`, ignorirajte kot komentarje.

## Oddaja domače naloge

Domačo nalogo rešujete na svojem klonu repozitorija predmeta. Priporočamo, da nalogo rešujete v posebni veji. Povezavo do repozitorija in veje, kjer ste reševali nalogo, oddate na spletni učilnici.

**NALOGO MORATE REŠEVATI SAMOSTOJNO. ČE NE VESTE, ALI DOLOČENA STVAR POMENI SODELOVANJE, RAJE VPRAŠAJTE!**
