---
jupytext:
  cell_metadata_filter: '-all'
  formats: 'md:myst'
  text_representation:
    extension: .md
    format_name: myst
    format_version: '0.8'
    jupytext_version: 1.5.0
kernelspec:
  display_name: Python 3
  language: python
  name: python3
---

# Naivni Bayesov klasifikator

Zanima nas, ali lahko iz opisa filma napovemo njegove žanre. Gre za _klasifikacijski problem_, saj želimo filme klasificirati v žanre, naša naloga pa je napisati ustrezen program, ki mu pravimo _klasifikator_.

## Predpriprava

```{code-cell}
# naložimo paket
import pandas as pd

# naložimo razpredelnico, s katero bomo delali
filmi = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/filmi.csv', index_col='id')
osebe = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/osebe.csv', index_col='id')
vloge = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/vloge.csv')
zanri = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/zanri.csv')
```

## Korenjenje besed

Da zadevo naredimo bolj obvladljivo, bomo opis predstavili le z množico korenov besed, ki se v opisu pojavljajo.

```{code-cell}
def koren_besede(beseda):
    beseda = ''.join(znak for znak in beseda if znak.isalpha())
    if not beseda:
        return '$'
    konec = len(beseda) - 1
    if beseda[konec] in 'ds':
        konec -= 1
    while konec >= 0 and beseda[konec] in 'aeiou':
        konec -= 1
    return beseda[:konec + 1]

def koreni_besed(niz):
    return pd.Series(sorted({
        koren_besede(beseda) for beseda in niz.replace('-', ' ').lower().split() if beseda
    }))
```

```{code-cell}
koreni_besed("In 1938, after his father Professor Henry Jones, Sr. goes missing while pursuing the Holy Grail, Indiana Jones finds himself up against Adolf Hitler's Nazis again to stop them obtaining its powers.")
```

## Bayesov izrek

Zanimala nas bo torej verjetnost, da ima film žanr $Ž_i$ ob pogoju, da njegov opis vsebuje korene $K_1, \ldots, K_m$, torej

$$P(Ž_i | K_1 \cap \cdots \cap K_n)$$

Pri tem se bomo poslužili Bayesovega izreka

$$P(A | B) = \frac{P(A \cap B)}{P(B)} = \frac{P(B | A) \cdot P(A)}{P(B)}$$

zaradi česar našemu klasifikatorju pravimo _Bayesov klasifikator_. Velja

$$P(Ž_i | K_1 \cap \cdots \cap K_n) = \frac{P(K_1 \cap \cdots \cap K_n | Ž_i) \cdot P(Ž_i)}{P(K_1 \cap \cdots \cap K_n)}$$

Nadalje si nalogo poenostavimo s predpostavko, da so pojavitve besed med seboj neodvisne. To sicer ni res, na primer ob besedi _treasure_ se bolj pogosto pojavlja beseda _hidden_ kot na primer _boring_, zato pravimo, da je klasifikator _naiven_. Ob tej predpostavki velja:

$$P(K_1 \cap \cdots \cap K_n | Ž_i) = P(K_1 | Ž_i) \cdot \cdots \cdot P(K_n | Ž_i)$$

oziroma

$$P(Ž_i | K_1 \cap \cdots \cap K_n) = \frac{P(K_1 | Ž_i) \cdot \cdots \cdot P(K_n | Ž_i) \cdot P(Ž_i)}{P(K_1 \cap \cdots \cap K_n)}$$

Filmu, katerega opis vsebuje korene $K_1, \dots, K_n$ bomo priredili tiste žanre $Ž_i$, pri katerih je dana verjetnost največja. Ker imenovalec ni odvisen od žanra, moramo torej za vsak $Ž_i$ izračunati le števec:

$$P(K_1 | Ž_i) \cdot \cdots \cdot P(K_n | Ž_i) \cdot P(Ž_i)$$

Vse te podatke znamo izračunati, zato se lahko lotimo dela.

Verjetnost posameznega žanra $P(Ž)$ izračunamo brez večjih težav:

```{code-cell}
verjetnosti_zanrov = zanri.groupby('zanr').size() / len(filmi)
verjetnosti_zanrov.sort_values()
```

Verjetnosti $P(K|Ž)$ bomo shranili v razpredelnico, v kateri bodo vrstice ustrezale korenom $K$, stolpci pa žanrom $Ž$. Najprej moramo poiskati vse filme, ki imajo žanr $Ž$, njihov opis pa vsebuje koren $K$. Vzemimo vse opise filmov:

```{code-cell}
filmi.opis
```

To vrsto nizov pretvorimo v vrsto množic besed. Uporabimo metodo `apply`, ki dano funkcijo uporabi na vsakem vnosu.

```{code-cell}
filmi.opis.apply(
    koreni_besed
)
```

Po nekaj [iskanja po internetu](https://stackoverflow.com/questions/30885005/pandas-series-of-lists-to-one-series) in masiranja pridemo do iskane razpredelnice:

```{code-cell}
koreni_filmov = filmi.opis.apply(
    koreni_besed
).stack(
).reset_index(
    level='id'
).rename(columns={
    'id': 'film',
    0: 'koren',
})
koreni_filmov
```

Razpredelnico združimo z razpredelnico žanrov, da dobimo razpredelnico korenov žanrov.

```{code-cell}
koreni_zanrov = pd.merge(
    koreni_filmov,
    zanri
)[['koren', 'zanr']]
koreni_zanrov
```

S pomočjo funkcije `crosstab` preštejemo, kolikokrat se vsaka kombinacija pojavi.

```{code-cell}
pojavitve_korenov_po_zanrih = pd.crosstab(koreni_zanrov.koren, koreni_zanrov.zanr)
pojavitve_korenov_po_zanrih
```

Iskane verjetnosti sedaj dobimo tako, da vsak stolpec delimo s številom filmov danega žanra. Da ne bomo dobili ničelne verjetnosti pri korenih, ki se v našem vzorcu ne pojavijo, verjetnost malenkost povečamo.

```{code-cell}
verjetnosti_korenov_po_zanrih = pojavitve_korenov_po_zanrih / zanri.groupby('zanr').size() + 0.001
```

Poglejmo, kaj so najpogostejši koreni pri nekaj žanrih:

```{code-cell}
verjetnosti_korenov_po_zanrih.Crime.sort_values(ascending=False).head(20)
```

```{code-cell}
verjetnosti_korenov_po_zanrih.Romance.sort_values(ascending=False).head(20)
```

```{code-cell}
verjetnosti_korenov_po_zanrih['Sci-Fi'].sort_values(ascending=False).head(20)
```

Žanre sedaj določimo tako, da za vsak žanr pomnožimo verjetnost žanra in pogojne verjetnosti vseh korenov, ki nastopajo v opisu filma.

```{code-cell}
def doloci_zanre(opis):
    faktorji_zanrov = verjetnosti_zanrov * verjetnosti_korenov_po_zanrih[
        verjetnosti_korenov_po_zanrih.index.isin(
            koreni_besed(opis)
        )
    ].prod()
    faktorji_zanrov /= max(faktorji_zanrov)
    return faktorji_zanrov.sort_values(ascending=False).head(5)
```

```{code-cell}
doloci_zanre('Alien space ship appears above Slovenia.')
```

```{code-cell}
doloci_zanre('A story about a young mathematician, who discovers her artistic side')
```
