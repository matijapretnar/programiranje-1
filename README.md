# Programiranje 1

V tem repozitoriju se zbirajo gradiva za predmet Programiranje 1 v 2. letniku matematike na [Fakulteti za matematiko in fiziko](https://www.fmf.uni-lj.si/).

## Povezave do prosojnic

Ker vam GitHub ob kliku na HTML datoteko namesto strani prikaže izvorno HTML kodo, je treba prosojnice pogledati s pomočjo strani <http://htmlpreview.github.io>. Tu so direktne povezave na prosojnice:

- [Uvod v OCaml](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/05-uvod-v-ocaml/predavanja/prosojnice.html)
- [Funkcije](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/06-funkcije/predavanja/prosojnice.html)
- [Definicije tipov](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/07-definicije-tipov/predavanja/prosojnice.html)
- [Učinki in čistost](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/08-ucinki-in-cistost/predavanja/prosojnice.html)
- [Iskalna drevesa](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/09-iskalna-drevesa/predavanja/prosojnice.html)
- [Spremenljive podatkovne strukture](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/10-spremenljive-podatkovne-strukture/predavanja/prosojnice.html)
- [Deli in vladaj](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/11-deli-in-vladaj/predavanja/prosojnice.html)
- [Dinamično programiranje](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/12-dinamicno-programiranje/predavanja/prosojnice.html)
- [Memoizacija v Pythonu](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/13-memoizacija-v-pythonu/predavanja/prosojnice.html)
- [Memoizacija v OCamlu](http://htmlpreview.github.io/?https://github.com/matijapretnar/programiranje-1/blob/master/14-memoizacija-v-ocamlu/predavanja/prosojnice.html)

## Namestitev programov

### 1\. Namestitev urejevalnika Visual Studio Code

Namestite si urejevalnik [Visual Studio Code](https://code.visualstudio.com/) in razširitev za [OCaml](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform). Razširitev deluje brez posebnih nastavitev za manjše datoteke, pri večjih projektih je potrebno nekaj nastavitev za pravilno prepoznavanje OCamla. Razširitev za [Python](https://marketplace.visualstudio.com/items?itemName=ms-python.python) bi morala biti že nameščena.

### 2\. Klon predmetnega repozitorija

Vse vaje (skupaj z rešitvami) so objavljene na [v tem repozitoriju](http://github.com/matijapretnar/programiranje-1/). Najenostavnejše jih boste reševali, če si [naredite svoj _fork_](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/configuring-a-remote-for-a-fork).

### 3\. Namestitev Pythona in OCamla

Za namestitev imate več možnosti: direktno na računalniku, z uporabo dockerja ali prek kakšnega od upravljalcev paketov. Če nič od tega ne dela, lahko OCaml uporabljate tudi iz spletnega brskalnika.

#### A) Direktna namestitev

##### Python

- Namestite si [Python](https://www.python.org/downloads/) in poskrbite, da je dodan v pot (`Path`)

##### OCaml

**Pozor:** Obvezno si namestite OCaml 4.XX.XX in **ne nameščajte različice 5.XX.XX**

- Linux: Namestite [OCaml](https://ocaml.org/docs/install.html) primeren za vaš sistem.
- MacOS: Namestite [OCaml](https://ocaml.org/docs/install.html) primeren za vaš sistem.
- Windows: Priporočamo, da uporabljate namestitev OCamla s pomočjo Dockerja v naslednjem razdelku.
OCaml lahko na windowse namestite tudi direktno, vendar je ta možnost vedno manj podprta.
[Za Windows uporabnike je na voljo preprostejša namestitev za [OCaml 4.02](http://protz.github.io/ocaml-installer/), ki jo uporabljajo tudi šolski računalniki.] Novejša različica [OCaml 4.12](https://fdopen.github.io/opam-repository-mingw/installation/) zahteva rahlo naprednejšo uporabo taskov v VSCode.

Taska za zagon Python in OCaml datotek sta na voljo na [repozitoriju predmeta](https://github.com/matijapretnar/programiranje-1/blob/master/.vscode/tasks.json).

Pri namestitvi na Windowsih brez Dockerja je potrebno nekoliko popraviti taske za OCaml:

```json
"label": "OCaml",  
"type": "shell",  
"command":
 "C:\\OCaml64\\usr\\local\\bin\\ocaml-env.exe exec -- C:\\OCaml64\\home\\???\\.opam\\4.12.0+mingw64c\\bin\\ocaml.exe -init \\\"${file}\\\""
```

kjer `???` nadomestite z uporabniškim imenom, s katerim ste prijavljeni na računalnik. Pri uporabniškem imenu pobrišete vse presledke in neangleške znake nadomestite z angleškimi verzijami (č -> c), npr. namesto `Katja Berčič`, uporabite `KatjaBercic`. Prav tako pri uporabi 32-bitne namestitve zamenjajte vse `64` z `32`.

Preverite, da ste namestili pravilno verzijo, saj namestitveni program posodabljajo. Odprite `C:\OCaml64\home\???\.opam\` in v task namesto `4.12.0+mingw64c` prilepite pravilno verzijo.

#### B) Namestitev z uporabo Dockerja

Namesto da si Python in OCaml namestite direktno na računalnik lahko uporabite docker kontainerje, ki poskrbijo, da so namestitve ločene. Slaba stran te namestitve je dejstvo, da (trenutno) porabi nekoliko več prostora na disku (prostor, ki ga potrebuje docker + 4.5 GB)

Pri namestitvi sledite navodilom iz <https://code.visualstudio.com/docs/devcontainers/containers>, kamor se lahko obrnete, če pride do kakšnih napak.

- Namestite si docker za vaš operacijski sistem: <https://docs.docker.com/get-docker/> in ga nastavite. Za sistem Windows si morate dodatno namestiti tudi WSL (<https://docs.microsoft.com/sl-si/windows/wsl/wsl2-kernel>), na kar vas ob prvem zagonu opozori tudi docker.
- V VSC si namestite razširitev [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers).
- Uredite si git v remote kontainerju <https://code.visualstudio.com/docs/devcontainers/containers#_working-with-git>.

Za uporabo klonirajte repozitorij predmeta in zaženite ukaz (`ctrl + shift + P`) `Dev Containers: Rebuild and Reopen in Container`.

Prvi zagon traja nekaj časa, saj mora naložiti celotno sliko, vsi naslednji zagoni pa so hitri.

Ko se projekt odpre, lahko normalno uporabljate taske, ki so na voljo na repozitoriju predmeta.

#### C) Namestitev s pomočjo upravljalcev paketov

- Če docker in Mingw ne delujeta, lahko uporabite [Diskuv](https://github.com/diskuv/dkml-installer-ocaml#readme)
- Če vas zanima delo na večjih ocaml projektih povezanih z medmrežjem, si lahko ocaml namestite prek ogrodja [esy](https://esy.sh/)

#### D) Uporaba v spletnem brskalniku

Če vam nobena od zgornjih možnosti ne dela, lahko uporabite enega od spletnih vmesnikov:

- <https://www.ocaml.org/play>
- <https://try.ocamlpro.com>
- <http://ocaml.besson.link>

### 4\. Preverjanje namestitve

Odprite poljubno OCaml datoteko (recimo [to](https://github.com/matijapretnar/programiranje-1/blob/master/05-uvod-v-ocaml/predavanja/primeri.ml)) in poženite ukaz `Run Tasks` in izberite task `OCaml`.

V konzolo vpišite `fakulteta 10;;` in preverite, če je rezultat pravilen.

### Pogoste težave in odgovori

- **Ukaz `pip` javi da `pip` ne obstaja, čeprav je `python` delujoč in dodan v `PATH`.**

    Problem je lahko v tem, da se pip ni pravilno namestil. Na windowsih lahko vidite, da v `Scripts` ni nobenih datotek. Gre za [znano težavo](https://bugs.python.org/issue40395) pri nameščanju pythona. Trenutno je to najlažje odpraviti tako, da namestite starejšo verzijo pythona (`3.6` ali nižje).

- **Terminal pri zagonu taskov na windowsu javi, da ne najde datoteke.**

    Nekatere verzije windowsa zahtevajo drugačno obliko zapisa za zagon. V obeh taskih trojne znake `\\\` zamenjajte z enojnimi `\`.  Del taska tako postane `\"${file}\"`.

- **Pri nameščanju OCaml-a dobim napako "Cygwin instalation is incomplete".**

    Izklopite vse antivirusne programe, odstranite obstoječo namestitev OCamla in namestitveni program za OCaml zaženite kot administrator.

- **Ob zagonu ocaml programa se nič ne zgodi, ali pa dobim napako "Error: Unbound module Stdlib"**

    Verjetno je prišlo do napake pri namestitvi cygwina. Možna rešitev je, da ročno nastavite sistemsko spremenljivko "OCAMLLIB" na mesto namestitve ocamla: "C:\OCaml64\home\UPORABNISKO_IME\.opam\4.12.0+mingw64c\lib\ocaml" (Kjer pravilno vpišete svoje uporabniško ime in verzijo ocamla).

- **Na linuxu dobim vse možne težave**

    Preverite, da imate nameščeno novejšo verzijo opama (vsaj `2.X`)

- **Na računalniku ne znam nastavit sistemskih spremenljivk**

    <https://superuser.com/a/1528432>

## Zapiski

Viri zapiskov se nahajajo v mapi `zapiski`. Za izdelavo HTML datotek si morate namestiti paket [`jupyter-book`](https://jupyterbook.org/). Nato pa pokličete

```bash
jupyter-book build zapiski
```

Če imate ustrezne pravice, lahko HTML najenostavneje objavite kar prek [GitHub pages](https://pages.github.com) tako, da si namestite še paket [`ghp-import`](https://github.com/c-w/ghp-import) in poženete

```bash
ghp-import --no-jekyll --no-history --force --push zapiski/_build/html
```
