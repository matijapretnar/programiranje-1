---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: Python 3
  language: python
  name: python3
---

# Memoizacija v Pythonu

Videli smo, da s pristopom od spodaj navzgor lahko učinkovito izračunamo rešitev problema dinamičnega programiranja, vendar se končna rešitev precej razlikuje od najbolj naravne rekurzivne rešitve. Če smo pripravljeni malo učinkovitosti žrtvovati za enostavnost in preglednost, lahko uporabimo _memoizacijo_ (ne memoRizacijo), kjer računalnik samodejno poskrbi za shranjevanje že izračunanih vrednosti.

Za primer si vzemimo funkcijo `kvadrat`, ki res ne porabi veliko časa za izračun, vendar vmes nekaj izpiše, zato bomo lahko spremljali, kolikokrat se pokliče.

```{code-cell}
def kvadrat(x):
    print('Računam', x)
    return x ** 2
```

```{code-cell}
kvadrat(10)
```

```{code-cell}
kvadrat(10)
```

Kot vidimo, v vsakem klicu funkcija izpiše niz in na novo izračuna `y`. Funkcijo lahko enostavno popravimo tako, da si že izračunane vrednosti shrani v slovar `kvadrati`.

```{code-cell}
kvadrati = {}
def mem_kvadrat(x):
    if x not in kvadrati:
        y = kvadrat(x)
        kvadrati[x] = y
    return kvadrati[x]
```

```{code-cell}
mem_kvadrat(10)
```

```{code-cell}
mem_kvadrat(10)
```

Kot vidimo, je funkcija ob drugem klicu vrnila že izračunano vrednost. Postopek lahko ponovimo tudi v splošnem s funkcijo, ki vzame (skoraj) poljubno funkcijo `f` in z njeno pomočjo definira funkcijo `mem_f`, ki hrani rezultate, nato pa `mem_f` vrne.

```{code-cell}
def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f
```

Na ta način iz običajne funkcije enostavno naredimo funkcijo, ki si zapomni že izračunane vrednosti.

```{code-cell}
mem_kvadrat2 = memoiziraj(kvadrat)
```

```{code-cell}
mem_kvadrat2(10)
```

```{code-cell}
mem_kvadrat2(10)
```

Poleg tega je slovar rezultatov lokalen, zato ga za razliko od slovarja `kvadrati` od zunaj ne moremo spreminjati.

```{code-cell}
kvadrati[2] = 5
```

```{code-cell}
mem_kvadrat(2)
```

V Pythonu je zadeva še bolj enostavna, saj lahko uporabimo dekoratorje. To so funkcije, ki jih lahko uporabimo na Pythonovih definicijah funkcij ter z dobljenimi rezultati povozimo prvotno definicijo. Zaporedje

```python
def f(x):
    ...
f = deko(f)
```

v katerem najprej definiramo funkcijo `f`, nato jo spremenimo s funkcijo `deko` in dobljeni rezultat shranimo nazaj pod ime `f`, lahko na kratko napišemo kot

```python
@deko
def f(x):
    ...
```

Tako lahko napišemo:

```{code-cell}
@memoiziraj
def kvadrat(x):
    print('Računam', x)
    return x ** 2
```

```{code-cell}
kvadrat(10)
```

```{code-cell}
kvadrat(10)
```

Na ta način smo z dodatkom samo ene vrstice poskrbeli, da si funkcija `kvadrat` hrani vrednosti. Podobno lahko definiramo tudi:

```{code-cell}
@memoiziraj
def stevilo_stolpov(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return sum(stevilo_stolpov(n - k) for k in [1, 2, 3])
```

```{code-cell}
stevilo_stolpov(100)
```

Dekorator `memoiziraj` je precej koristen, zato že obstaja v Pythonovi standardni knjižnjici `functools`. Od verzije Python 3.9 ga najdemo pod imenom `cache`, ki podpira tudi funkcije z več argumenti in podobno. Že pred verzijo 3.9 pa je obstajala splošnejša različica `lru_cache` (_least recently used_), ki sprejme število `maxsize` nazadnje izračunanih vrednosti, ki naj jih še hrani. Če za `maxsize` podamo `None`, si bo računalnik hranil vse do sedaj izračunane vrednosti.

```{code-cell}
from functools import lru_cache
# oz. from functools import cache
```

```{code-cell}
@lru_cache(maxsize=None)
# oz. @cache
def stevilo_stolpov(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return sum(stevilo_stolpov(n - k) for k in [1, 2, 3])
```

```{code-cell}
stevilo_stolpov(100)
```

Kot smo namignili, ne moremo memoizirati čisto vsake funkcije. Težavo povzročajo tisti argumenti, ki jih ne moremo shraniti v slovar. Na primer, poskusimo uporabiti memoizacijo pri iskanju najcenejše poti v matriki.

```{code-cell}
@lru_cache(maxsize=None)
def cena_najcenejse_poti_iz_polja(matrika, i, j):
    m, n = len(matrika), len(matrika[0])
    if i < n - 1 and j < m - 1:
        cena_navzdol = cena_najcenejse_poti_iz_polja(matrika, i + 1, j)
        cena_desno = cena_najcenejse_poti_iz_polja(matrika, i, j + 1)
        return matrika[i][j] + min(cena_navzdol, cena_desno)
    elif i < n - 1:
        cena_navzdol = cena_najcenejse_poti_iz_polja(matrika, i + 1, j)
        return matrika[i][j] + cena_navzdol
    elif j < m - 1:
        cena_desno = cena_najcenejse_poti_iz_polja(matrika, i, j + 1)
        return matrika[i][j] + cena_desno
    else:
        return matrika[i][j]
```

```{code-cell}
m = [[131, 673, 234, 103, 18],
     [201, 96, 342, 965, 150],
     [630, 803, 746, 422, 111],
     [537, 699, 497, 121, 956],
     [805, 732, 524, 37, 331]]
```

```{code-cell}
:tags: ["raises-exception"]
cena_najcenejse_poti_iz_polja(m, 0, 0)
```

Matrike Python ne more shraniti v slovar. Vendar nam je v resnici ni treba, ker se spreminjata le argumenta `i` in `j`. Tako lahko naredimo pomožno funkcijo, ki je odvisna samo od teh dveh argumentov, nato pa dekorator uporabimo na njej:

```{code-cell}
def cena_najcenejse_poti(matrika):
    m, n = len(matrika), len(matrika[0])
    @lru_cache(maxsize=None)
    def pomozna(i, j):
        if i < n - 1 and j < m - 1:
            cena_navzdol = pomozna(i + 1, j)
            cena_desno = pomozna(i, j + 1)
            return matrika[i][j] + min(cena_navzdol, cena_desno)
        elif i < n - 1:
            cena_navzdol = pomozna(i + 1, j)
            return matrika[i][j] + cena_navzdol
        elif j < m - 1:
            cena_desno = pomozna(i, j + 1)
            return matrika[i][j] + cena_desno
        else:
            return matrika[i][j]
    return pomozna(0, 0)
```

```{code-cell}
cena_najcenejse_poti(m)
```
