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

# Knjižnica Pandas

Spodaj je pregled najosnovnejših metod, ki jih ponuja knjižnica Pandas. Vsaka od naštetih metod ponuja še cel kup dodatnih možnosti, ki so natančno opisane v [uradni dokumentaciji](http://pandas.pydata.org/pandas-docs/stable/). Z branjem dokumentacije se vam seveda najbolj splača začeti pri [uvodih](http://pandas.pydata.org/pandas-docs/stable/tutorials.html).


## Predpriprava

```{code-cell}
# naložimo paket
import pandas as pd

# naložimo razpredelnico, s katero bomo delali
filmi = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/filmi.csv', index_col='id')

# ker bomo delali z velikimi razpredelnicami, povemo, da naj se vedno izpiše le 20 vrstic
pd.options.display.max_rows = 20
```

## Osnovni izbori elementov razpredelnic


Z metodo `.head(n=5)` pogledamo prvih `n`, z metodo `.tail(n=5)` pa zadnjih `n` vrstic razpredelnice.

```{code-cell}
filmi.head(10)
```

```{code-cell}
filmi.tail()
```

Z rezinami pa dostopamo do izbranih vrstic.

```{code-cell}
filmi[3:10:2]
```

Z indeksiranjem razpredelnice dostopamo do posameznih stolpcev.

```{code-cell}
filmi['ocena']
```

Do stolpcev pogosto dostopamo, zato lahko uporabimo tudi krajši zapis.

```{code-cell}
filmi.ocena
```

Če želimo več stolpcev, moramo za indeks podati seznam vseh oznak.

```{code-cell}
filmi[['naslov', 'ocena']]
```

Do vrednosti z indeksom `i` dostopamo z `.iloc[i]`, do tiste s ključem `k` pa z `.loc[k]`.

```{code-cell}
filmi.iloc[120]
```

```{code-cell}
filmi.loc[97576]
```

## Filtriranje


Izbor določenih vrstic razpredelnice naredimo tako, da za indeks podamo stolpec logičnih vrednosti, ki ga dobimo z običajnimi operacijami. V vrnjeni razpredelnici bodo ostale vrstice, pri katerih je v stolpcu vrednost `True`.

```{code-cell}
filmi.ocena >= 8
```

```{code-cell}
filmi[filmi.ocena >= 9.3]
```

```{code-cell}
filmi[(filmi.leto > 2010) & (filmi.ocena > 8) | (filmi.ocena < 5)]
```

### Naloga

Poiščite filme, ki si jih želimo izogniti za vsako ceno, torej tiste, ki so daljši od dveh ur in imajo oceno pod 4.

```{code-cell}
filmi[(filmi.dolzina > 120) & (filmi.ocena < 4) & (filmi.glasovi > 50000)]
```

## Urejanje


Razpredelnico urejamo z metodo `.sort_values`, ki ji podamo ime ali seznam imen stolpcev, po katerih želimo urejati. Po želji lahko tudi povemo, kateri stolpci naj bodo urejeni naraščajoče in kateri padajoče.

```{code-cell}
filmi.sort_values('dolzina')
```

```{code-cell}
# najprej uredi padajoče po oceni, pri vsaki oceni pa še naraščajoče po letu
filmi.sort_values(['ocena', 'leto'], ascending=[False, True])
```

## Združevanje


Z metodo `.groupby` ustvarimo razpredelnico posebne vrste, v katerem so vrstice združene glede na skupno lastnost.

```{code-cell}
filmi_po_letih = filmi.groupby('leto')
```

```{code-cell}
filmi_po_letih
```

```{code-cell}
# povprečna ocena vsakega leta
filmi_po_letih.ocena.mean()
```

Če želimo, lahko združujemo tudi po izračunanih lastnostih. Izračunajmo stolpec in ga shranimo v razpredelnico.

```{code-cell}
filmi['desetletje'] = 10 * (filmi.leto // 10)
```

```{code-cell}
filmi
```

```{code-cell}
filmi_po_desetletjih = filmi.groupby('desetletje')
```

Preštejemo, koliko filmov je bilo v vsakem desetletju. Pri večini stolpcev dobimo iste številke, ker imamo v vsakem stolpcu enako vnosov. Če kje kakšen podatek manjkal, je številka manjša.

```{code-cell}
filmi_po_desetletjih.count()
```

Če želimo dobiti le število članov posamezne skupine, uporabimo metodo `.size()`. V tem primeru dobimo le stolpec, ne razpredelnice.

```{code-cell}
filmi_po_desetletjih.size()
```

Pogledamo povprečja vsakega desetletja. Dobimo povprečno leto, dolžino, ocene in zaslužek. Povprečnega naslova ne dobimo, ker se ga ne da izračunati, zato ustreznega stolpca ni.

```{code-cell}
filmi_po_desetletjih.mean()
```

### Naloga

Izračunajte število filmov posamezne dolžine, zaokrožene na 5 minut.


## Risanje grafov

Običajen graf dobimo z metodo `plot`. Uporabljamo ga, kadar želimo prikazati spreminjanje vrednosti v odvisnosti od zvezne spremenljivke. Naša hipoteza je, da so zlata leta filma mimo. Graf to zanika.

```{code-cell}
filmi[filmi.ocena > 9].groupby('desetletje').size().plot()
```

Razsevni diagram dobimo z metodo `plot.scatter`. Uporabljamo ga, če želimo ugotoviti povezavo med dvema spremenljivkama.

```{code-cell}
filmi.plot.scatter('ocena', 'metascore')
```

```{code-cell}
filmi[filmi.dolzina < 250].plot.scatter('dolzina', 'ocena')
```

Stolpčni diagram dobimo z metodo `plot.bar`. Uporabljamo ga, če želimo primerjati vrednosti pri diskretnih (običajno kategoričnih) spremenljivkah. Pogosto je koristno, da graf uredimo po vrednostih.

```{code-cell}
filmi.sort_values('zasluzek', ascending=False).head(20).plot.bar(x='naslov', y='zasluzek')
```

### Naloga

Narišite grafe, ki ustrezno kažejo:
- Povezavo med IMDB in metascore oceno
- Spreminjanje povprečne dolžine filmov skozi leta


## Stikanje

```{code-cell}
osebe = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/osebe.csv', index_col='id')
vloge = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/vloge.csv')
zanri = pd.read_csv('../02-zajem-podatkov/predavanja/obdelani-podatki/zanri.csv')
```

Razpredelnice stikamo s funkcijo `merge`, ki vrne razpredelnico vnosov iz obeh tabel, pri katerih se vsi istoimenski podatki ujemajo.

```{code-cell}
vloge[vloge.film == 12349]
```

```{code-cell}
zanri[zanri.film == 12349]
```

```{code-cell}
pd.merge(vloge, zanri).head(20)
```

V osnovi vsebuje staknjena razpredelnica le tiste vnose, ki se pojavijo v obeh tabelah. Temu principu pravimo notranji stik (_inner join_). Lahko pa se odločimo, da izberemo tudi tiste vnose, ki imajo podatke le v levi tabeli (_left join_), le v desni tabeli (_right join_) ali v vsaj eni tabeli (_outer join_). Če v eni tabeli ni vnosov, bodo v staknjeni tabeli označene manjkajoče vrednosti. Ker smo v našem primeru podatke jemali iz IMDBja, kjer so za vsak film določeni tako žanri kot vloge, do razlik ne pride.


Včasih želimo stikati tudi po stolpcih z različnimi imeni. V tem primeru funkciji `merge` podamo argumenta `left_on` in `right_on`.

```{code-cell}
pd.merge(pd.merge(vloge, zanri), osebe, left_on='oseba', right_on='id')
```

Poglejmo, katera osebe so nastopale v največ komedijah.

```{code-cell}
zanri_oseb = pd.merge(pd.merge(vloge, zanri), osebe, left_on='oseba', right_on='id')
zanri_oseb[
    (zanri_oseb.zanr == 'Comedy') &
    (zanri_oseb.vloga == 'I')
].groupby(
    'ime'
).size(
).sort_values(
    ascending=False
).head(20)
```

### Naloga

- Izračunajte povprečno oceno vsakega žanra.
- Kateri režiserji snemajo najdonosnejše filme?
