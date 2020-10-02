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

# Regularni izrazi

Običajno moramo podatke, ki jih želimo analizirati, najprej prečistiti. Na primer, svetovni splet je bogat vir podatkov, vendar so ti dostikrat dostopni le v formatu HTML, ki poleg koristne vsebine vsebuje še marsikaj. Recimo, da nas zanimajo podatki o [250 filmih z največ glasovi na strani IMDB](https://www.imdb.com/search/title/?sort=num_votes,desc&title_type=feature&count=250). Vidimo, da stran ponuja veliko koristnih podatkov: naslov, leto izida, dolžno, žanre, ocene, igralce, opise, ...

![250 najbolj znanih filmov](datoteke/250-najbolj-znanih-filmov.png)

Če pa v brskalniku shranimo izvorno kodo in HTML datoteko odpremo, pa je podatke težko najti.

```{code-cell}
with open('datoteke/250-najbolj-znanih-filmov.html') as f:
    html = f.read()

print(html[:1000])
```

Pomagajmo si s pomožno funkcijo, ki poišče začetke in konce vseh neprekrivajočih se pojavitev danega niza v besedilu.

```{code-cell}
def vse_pojavitve(besedilo: str, iskani_niz: str):
    konec_pojavitve = 0
    while True:
        try:
            zacetek_pojavitve = besedilo.index(iskani_niz, konec_pojavitve)
            konec_pojavitve = min(zacetek_pojavitve + len(iskani_niz), len(besedilo))
            yield zacetek_pojavitve, konec_pojavitve
        except ValueError:
            break
```

```{code-cell}
list(vse_pojavitve('Ena sama je, mama!', 'ma')
```

Če želimo, lahko vsako pojavitev prikažemo v njenem kontekstu:

```{code-cell}
def pokazi_vse_pojavitve(besedilo: str, iskani_niz: str, velikost_konteksta=50):
    for zacetek, konec in vse_pojavitve(besedilo, iskani_niz):
        zacetek_konteksta = max(zacetek - velikost_konteksta, 0)
        konec_konteksta = min(konec + velikost_konteksta, len(besedilo))
        print(besedilo[zacetek_konteksta:konec_konteksta])
        print((zacetek - zacetek_konteksta) * ' ' + (konec - zacetek) * '^')
```

```{code-cell}
pokazi_vse_pojavitve('Ena sama je, mama!', 'ma')
```

Recimo, da nas zanimajo podatki o Vojni zvezd:

```{code-cell}
pokazi_vse_pojavitve(html.replace('\n', ''), 'Star Wars')
```

V datoteki najdemo kar nekaj pojavitev, za vsako epizodo po dve: eno iz prikaza naslova in eno iz opisa slike. Vidimo, da so vsi naslovi podobne oblike: na začetku je značka `<a href="/title/tt0123456/?ref_=adv_li_tt">`, pri čemer se šifra spreminja od filma do filma, na koncu je `</a>`, med njima pa je naslov filma. Če bi se zelo potrudili, bi lahko spisali program, ki iz takega niza izlušči šifro in naslov.

```{code-cell}
def izlusci_sifro_in_naslov(niz):
    pred_sifro = '<a href="/title/tt'
    med_sifro_in_naslovom = '/?ref_=adv_li_tt">'
    za_naslovom = '</a>'
    zacetek_sifre = niz.index(pred_sifro) + len(pred_sifro)
    konec_sifre = niz.index(med_sifro_in_naslovom)
    zacetek_naslova = konec_sifre + len(med_sifro_in_naslovom)
    konec_naslova = niz.index(za_naslovom)
    sifra = int(niz[zacetek_sifre:konec_sifre])
    naslov = niz[zacetek_naslova:konec_naslova]
    return sifra, naslov
```

```{code-cell}
izlusci_sifro_in_naslov('<a href="/title/tt0076759/?ref_=adv_li_tt">Star Wars: Episode IV - A New Hope</a>')
```

```{code-cell}
izlusci_sifro_in_naslov('<a href="/title/tt0086190/?ref_=adv_li_tt">Star Wars: Episode VI - Return of the Jedi</a>')
```

Podobno bi lahko naredili še za leto izida, dolžino in ostalo, vendar mora obstajati boljši način. Mi si bomo pogledali dva. Prvi način so regularni izrazi - ti so univerzalno (z manjšimi dialekti) razširjeni zapis vzorcev nizov, ki jih lahko uporabljamo za delo s kakršnim koli besedilom. Regularne izraze podpirajo praktično vsi programski jeziki in naprednejši urejevalniki besedil. Zaradi vseobče uporabnosti se bomo regularnim izrazom bolj posvetili, niso pa vedno najboljše orodje pri roki, saj zaradi splošnosti ne izkoristijo vse strukture, ki je na voljo. Zato si bomo ogledali še drugi način, knjižnico [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/), ki je namenjena analizi HTML datotek.

## Regularni izrazi v Pythonu

Za delo z regularnimi izrazi (ali regeksi) v Pythonu uporabljamo knjižnico [`re`](https://docs.python.org/3/library/re.html), ki je vključen v vsako različico Pythona. V njej je najbolj osnovna funkcija `search`, ki sprejme vzorec in besedilo, v katerem iščemo, ter vrne prvo pojavitev, predstavljeno z objektom razreda `re.Match`.

```{code-cell}
import re
re.search('ma', 'Ena sama je, mama!')
```

V objektu imamo dostop do začetka in konca pojavitve in njene vsebine. Uporabimo lahko tudi funkcijo `re.finditer`, ki vrne iterator po vseh pojavitvah:

```{code-cell}
list(re.finditer('ma', 'Ena sama je, mama!'))
```

S pomočjo te funkcije lahko na veliko bolj enostaven način napišemo funkcijo `vse_pojavitve`.

```{code-cell}
def vse_pojavitve(besedilo: str, vzorec: str):
    for pojavitev in re.finditer(vzorec, besedilo):
        yield pojavitev.start(), pojavitev.end()
```

```{code-cell}
pokazi_vse_pojavitve('Ena sama je, mama!', 'ma')
```

Seveda pa regularni izrazi pokažejo svojo pravo moč, ko začnemo uporabljati še ostale vzorce. Vse lahko najdete v [uradni dokumentaciji](https://docs.python.org/3/library/re.html#regular-expression-syntax), mi pa si poglejmo najbolj pogoste.

### Vzorci za znake

Katerikoli znak predstavimo s piko:

```{code-cell}
pokazi_vse_pojavitve('Ena sama je, mama!', '.a')
```

```{code-cell}
pokazi_vse_pojavitve('Ena sama je, mama!', '.m')
```

Če se želimo omejiti na posamezne znake ali posamezen razpon zaporednih znakov, jih naštejemo med oglatimi oklepaji:

```{code-cell}
pokazi_vse_pojavitve('ata, mama, teta, stric', '.[aeiou].')
```

```{code-cell}
pokazi_vse_pojavitve('ata, mama, teta, stric', '.[aeiou][a-z]')
```

Če kot prvi znak v oglatih oklepajih damo `^`, dobimo komplement:

```{code-cell}
pokazi_vse_pojavitve('ata, mama, teta, stric', '.[^aeiou].')
```

### Kvantifikatorji

Z `*` označimo poljubno mnogo ponovitev danega vzorca:

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd*a')
```

Če želimo, da se vzorec pojavi vsaj enkrat, uporabimo `+`:

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd+a')
```

Kvantifikatorja `*` in `+` sta požrešna, kar pomeni, da poskusita zajeti kolikor znakov lahko:

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd.*a')
```

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd.+a')
```

Če želimo najti najkrajše možne pojavitve, moramo na koncu dodati še `?`:

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd.*?a')
```

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'd.+?a')
```

Če uporabimo samo `?`, to pomeni morebitno pojavitev vzorca:

```{code-cell}
pokazi_vse_pojavitve('Oddal sem davčno napoved', 'da?')
```

Kvantifikatorji `*`, `+` in `?` so posebni primeri kvantifikatorja `{m,n}`, ki predstavlja katerokoli število ponovitev med `m` in `n`, pri čemer lahko kakšno izmed meja tudi izpustimo. Tako je kvantifikator `*` okrajšava za `{0,}`, kvantifikator `+` okrajšava za `{1,}`, kvantifikator `?` pa okrajšava za `{0,1}`.

```{code-cell}
pokazi_vse_pojavitve('"Brrrr, brrrrrr, brrr, brrrrrrr," je drgetal od mraza.', '[Bb]r{4,5}')
```

```{code-cell}
pokazi_vse_pojavitve('"Brrr, brrrrrr, brrr, brrrrrrr," je drgetal od mraza.', '[Bb]r{4,}')
```

```{code-cell}
pokazi_vse_pojavitve('"Brrr, brrrrrr, brrr, brrrrrrr," je drgetal od mraza.', '[Bb]r{,5}')
```

### Posebni znaki

### Skupine

## Metode za delo z regularnimi izrazi

## Knjižnica Beautiful Soup
