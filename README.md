# Programiranje 1

V tem repozitoriju se zbirajo gradiva za predmet Programiranje 1 v 2. letniku matematike na [Fakulteti za matematiko in fiziko](https://www.fmf.uni-lj.si/).

## Namestitev delovnega okolja

### 1\. Namestitev urejevalnika Visual Studio Code

Namestite si urejevalnik [Visual Studio Code (VS Code)](https://code.visualstudio.com/).

### 2\. Klon predmetnega repozitorija

Vse vaje (skupaj z rešitvami) so objavljene [v tem repozitoriju](http://github.com/matijapretnar/programiranje-1/). Najenostavneje jih boste reševali, če si [naredite svoj _fork_](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/configuring-a-remote-for-a-fork) ter ga klonirate na svoj računalnik.

### 3\. Namestitev OCamla & Pythona

#### Windows

Ker je podpora za OCaml na Windowsih že desetletja v povojih, je najpreprostejša namestitev prek [Dockerja](https://www.docker.com/), ki na vašem računalniku ustvari neke vrste virtualni računalnik. Da tega ni potrebno ročno nastavljati, lahko izkoristite možnost [Dev Containers](<https://code.visualstudio.com/docs/devcontainers/containers), ki v VS Code samodejno namesti vse potrebno na podlagi nastavitev iz imenika `.devcontainer`, vključno z OCamlom, Pythonom, razširitvami za VS Code in vsemi potrebnimi paketi. Slaba stran te namestitve je dejstvo, da (trenutno) porabi nekoliko več prostora na disku (Docker + približno 4.5 GB).

- Namestite si Docker za vaš operacijski sistem: <https://docs.docker.com/get-docker/> in ga nastavite. Za sistem Windows si morate dodatno namestiti tudi WSL (<https://docs.microsoft.com/sl-si/windows/wsl/wsl2-kernel>), na kar vas ob prvem zagonu opozori tudi Docker.
- V VS Code si namestite razširitev [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers).
- V VS Code odprite prej klonirani imenik predmeta in zaženite ukaz (`ctrl + shift + P`) `Dev Containers: Rebuild and Reopen in Container`. Prvi zagon lahko traja okoli 20 minut, saj se mora prenesti celoten sistem. Vsi naslednji zagoni pa so hitri.

#### Linux & macOS

Tudi tu priporočamo zgoraj opisano namestitev prek Dockerja, saj vključuje vse razširitve in pakete, ki jih potrebujete pri predmetu. Če pa želite namestiti OCaml neposredno, sledite navodilom na [uradni strani](https://ocaml.org/docs/install.html). Python bi morali imeti že nameščen. V datoteki [`Dockerfile`](.devcontainer/Dockerfile) lahko pogledate, katere Python (`pip install …`) in OCaml pakete (`opam install …`) morate še namestiti.

#### Zasilna rešitev

Če vam OCamla ne uspe namestiti, ga lahko uporabljate tudi iz spletnega brskalnika:

- <https://www.ocaml.org/play> (uradna različica, najenostavnejši vmesnik, OCaml 5.0.0)
- <http://ocaml.besson.link> (več možnosti, verzija OCaml 4.13.1)
- <https://try.ocamlpro.com> (delo z več datotekami, malo bolj okoren, OCaml 4.14.1)

Z [namestitvijo Pythona](https://www.python.org/downloads/) ne bi smeli imeti večjih težav.

### 4\. Preverjanje namestitve

Ustvarite datoteko `primer.ml` z vsebino:

```ocaml
let rec fakulteta =
  function
  | 0 -> 1
  | n -> n * fakulteta (n - 1)
```

Nato zaženite ukaz `Run Tasks` in izberite opravilo `OCaml`. V konzolo vpišite `fakulteta 10;;` in preverite, ali je rezultat pravilen.

## Zapiski

Viri zapiskov se nahajajo v mapi `zapiski`. Za izdelavo HTML datotek si morate namestiti paket [`jupyter-book`](https://jupyterbook.org/). Nato uporabite ukaz:

```bash
jupyter-book build zapiski
```

Če imate ustrezne pravice, lahko HTML najpreprosteje objavite prek [GitHub Pages](https://pages.github.com) tako, da si namestite še paket [`ghp-import`](https://github.com/c-w/ghp-import) in poženete:

```bash
ghp-import --no-jekyll --no-history --force --push zapiski/_build/html
```
