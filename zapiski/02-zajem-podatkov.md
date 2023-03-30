# Zajem podatkov

Zdaj, ko znamo analizirati eno stran filmov, bi radi to ponovili za več strani. Pri tem seveda teh strani ne bomo zajemale ročno, ampak bomo napisali program, ki bo to storil namesto nas.

## Knjižnica `requests`

Kljub temu, da Pythonova standardna knjižnica že vsebuje modul `urllib` za delo s spletnimi stranmi, bomo uporabili knjižnico [`requests`](http://docs.python-requests.org/), ki je bolj enostavna za uporabo, hkrati pa podpira stvari, kot so certifikati, preusmeritve in podobno. Ko si knjižnico enkrat namestimo, lahko strani preprosto preberemo z ukazom

```python
import requests
r = requests.get('http://www.fmf.uni-lj.si/')
statusna_koda = r.status_code
vsebina = r.text
```

Podatke moramo zajeti s strani oblike `https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start=<START>`, kjer je `<START>` enak 1, 251, 501 in tako naprej do 9751. Da nam strani ne bo treba zajemati večkrat (ter s tem izgubljati časa in tvegati, da bo strežnik začel zavračati naše zahteve), bomo vsako izmed njih shranili v datoteko:

```python
st_strani = 40
na_stran = 250
for stran in range(st_strani):
    url = f"https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count={na_stran}&start={1 + stran * na_stran}"
    odziv = requests.get(url)
    if odziv.status_code == 200:
        print(url)
        with open(f"stran-{stran}.html", "w") as f:
            f.write(odziv.text)
    else:
        print("Prišlo je do napake")
```

Če to sestavimo skupaj s programom za analizo strani iz prejšnjega poglavja dobimo seznam 10000 slovarjev, na primer:

```python
filmi = [
    {
        "sifra": 76759,
        "naslov": "Vojna zvezd",
        "leto": 1977,
        "ocena": 8.6,
        "reziser": "George Lucas",
        "igralci": [...],
    },
    {
        "sifra": 80684,
        "naslov": "Imperij vrača udarec",
        "leto": 1980,
        "ocena": 8.7,
        "reziser": "Irvin Kershner",
        "igralci": [...],
    },
    {
        "sifra": 82971,
        "naslov": "Lov za izgubljenim zakladom",
        "leto": 1981,
        "ocena": 8.4,
        "reziser": "Steven Spielberg",
        "igralci": [...],
    },
    {
        "sifra": 86190,
        "naslov": "Vrnitev jedija",
        "leto": 1983,
        "ocena": 8.3,
        "reziser": "Richard Marquand",
        "igralci": [...],
    },
    {
        "sifra": 87469,
        "naslov": "Indiana Jones in tempelj smrti",
        "leto": 1984,
        "ocena": 7.6,
        "reziser": "Steven Spielberg",
        "igralci": [...],
    },
    {
        "sifra": 97576,
        "naslov": "Indiana Jones in Zadnji križarski pohod",
        "leto": 1989,
        "ocena": 8.2,
        "reziser": "Steven Spielberg",
        "igralci": [...],
    },
]
```

## Zapis CSV

Prečiščene podatke bi lahko shranili v zapis JSON, vendar jih bomo raje shranili v tabelarično obliko, ki je za analizo primernejša od hierarhične. Za to uporabimo zapis CSV (Comma-Separated Values), ki je univerzalni zapis podatkov v besedilni datoteki. Datoteke CSV lahko odpremo v praktično vsakem programu za delo s podatki. V zapisu CSV vsaka vrstica predstavlja vnos, v katerem so polja ločena z vejico. Prvo vrstico običajno uporabimo za imena polj. Na primer, tabelo:

| id | naslov | leto | ocena
| :-: | :- | :-: | :-:
| 76759 | Vojna zvezd | 1977 | 8.6
| 80684 | Imperij vrača udarec | 1980 | 8.7
| 82971 | Lov za izgubljenim zakladom | 1981 | 8.4
| 86190 | Vrnitev jedija | 1983 | 8.3
| 87469 | Indiana Jones in tempelj smrti | 1984 | 7.6
| 97576 | Indiana Jones in Zadnji križarski pohod | 1989 | 8.2

bi v zapisu CSV pisali kot:

```csv
id,naslov,leto,ocena
76759,Vojna zvezd,1977,8.6
80684,Imperij vrača udarec,1980,8.7
82971,Lov za izgubljenim zakladom,1981,8.4 
86190,Vrnitev jedija,1983,8.3
87469,Indiana Jones in tempelj smrti,1984,7.6 
97576,Indiana Jones in Zadnji križarski pohod,1989,8.2 
```

Da se izognemo robnim primerom kot vejice v naslovih, CSV datotek ne bomo pisali na roke temveč prek knjižnice `csv`. Iz zgornjega seznama slovarjev lahko CSV datoteko naredimo z:

```python
import csv

with open("filmi.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["id", "naslov", "leto", "ocena"])
    for film in podatki:
        writer.writerow([film["sifra"], film["naslov"], film["leto"], film["ocena"]])
```

Če želimo, lahko vsako vrstico podamo kar neposredno s slovarjem:

```python
with open("filmi.csv", "w") as f:
    writer = csv.DictWriter(f, fieldnames=["id", "naslov", "leto", "ocena"])
    writer.writeheader()
    for film in podatki:
        writer.writerow(film)
```

Podobno lahko s knjižnico CSV tudi beremo datoteke.

## Normalizacija podatkov

Kako bi v tabeli shranili podatke o režiserju? Očitna rešitev je, da v tabelo dodamo še stolpec z režiserjem, na primer:

| id | naslov | leto | ocena | reziser
| :-: | :- | :-: | :-: | :-:
| 76759 | Vojna zvezd | 1977 | 8.6 | George Lucas
| 80684 | Imperij vrača udarec | 1980 | 8.7 | Irvin Kershner
| 82971 | Lov za izgubljenim zakladom | 1981 | 8.4 | Steven Spilberg
| 86190 | Vrnitev jedija | 1983 | 8.3 | Richard Marquand
| 87469 | Indiana Jones in tempelj smrti | 1984 | 7.6 | Steven Spielberg
| 97576 | Indiana Jones in Zadnji križarski pohod | 1989 | 8.2 | Steven Spielberg
| ... | ... | ... | ... | ...

Vendar ta rešitev ni najboljša. Prva težava je, da je podvajanje podatkov potratno (kaj šele, da bi si ob vsakem režiserju želeli shraniti še življenjepis). Še večja težava pa je, da pri podvajanju podatki niso nujno konsistentni. Na primer, pri tretjem filmu je ime režiserja napisano napačno.

Namesto tega bomo podatke o režiserjih shranili v ločeno tabelo:

| id | ime | datum_rojstva | zivljenjepis
| :-: | :-: | :-: | :-:
| 184 | George Lucas | 1944-05-14 | ...
| 229 | Steven Spielberg | 1946-12-18 | ...
| 449984 | Irvin Kershner | 1923-04-29 | ...
| 549658 | Richard Marquand | 1937-09-22 | ...
| ... | ... | ... | ...

V tabeli filmov bomo shranili samo številko režiserja:

| id | naslov | leto | ocena | reziser
| :-: | :- | :-: | :-: | :-:
| 76759 | Vojna zvezd | 1977 | 8.6 | 184
| 80684 | Imperij vrača udarec | 1980 | 8.7 | 449984
| 82971 | Lov za izgubljenim zakladom | 1981 | 8.4 | 229
| 86190 | Vrnitev jedija | 1983 | 8.3 | 549658
| 87469 | Indiana Jones in tempelj smrti | 1984 | 7.6 | 229
| 97576 | Indiana Jones in Zadnji križarski pohod | 1989 | 8.2 | 229
| ... | ... | ... | ... | ...

Tako vsako podatek zapišemo samo enkrat, zato ni več težav ne s prostorom, ne s konsistentnostjo podatkov. Pravimo, da so podatki _normalizirani_.

Kako pa bi shranili podatke o igralcih, ki jih je lahko več? (V resnici je tudi režiserjev lahko več, vendar to zaenkrat zanemarimo.) Vemo že, da bomo igralce dodali v tabelo oseb:

| id | ime | datum_rojstva | zivljenjepis
| :-: | :-: | :-: | :-:
| 125 | Sean Connery | 1930-08-25 | ...
| 148 | Harrison Ford | 1942-07-13 | ...
| 184 | George Lucas | 1944-05-14 | ...
| 229 | Steven Spielberg | 1946-12-18 | ...
| ... | ... | ... | ...

V tabeli filmov bi lahko shranili seznam številk igralcev:

| id | naslov | leto | ocena | reziser | igralci
| :-: | :- | :-: | :-: | :-: | :-:
| 76759 | Vojna zvezd | 1977 | 8.6 | 184 | 434,148,402
| 80684 | Imperij vrača udarec | 1980 | 8.7 | 449984 | 434,148,402
| 82971 | Lov za izgubljenim zakladom | 1981 | 8.4 | 229 | 148,261
| 86190 | Vrnitev jedija | 1983 | 8.3 | 549658 | 434,148,402
| 87469 | Indiana Jones in tempelj smrti | 1984 | 7.6 | 229 | 148
| 97576 | Indiana Jones in Zadnji križarski pohod | 1989 | 8.2 | 229 | 148,125
| | ... | ... | ... | ... | ...

Vendar bi bilo po takih podatkih težko iskati. Namesto tega bomo uvedli dodatno _povezovalno tabelo_ vlog, v kateri vsak vnos sporoča, kateri igralec je igral v katerem filmu:

| film | oseba
| :-: | :-:
| 76759 | 434
| 76759 | 148
| 76759 | 402
| 80684 | 434
| 80684 | 148
| ... | ...

Če tabelo opremimo še s tipom vloge (I - igralec, R - režiser), lahko vanjo shranimo tako podatke o igralcih kot o režiserjih:

| film | oseba | vloga
| :-: | :-: | :-:
| 76759 | 148 | I
| 76759 | 184 | R
| 76759 | 402 | I
| 76759 | 434 | I
| 80684 | 148 | I
| 80684 | 434 | I
| 80684 | 449984 | R
| 82971 | 229 | R
| 86190 | 549658 | R
| 87469 | 229 | R
| 97576 | 229 | R
| ... | ... | ...

Če bi kakšna oseba tako režirala nek film kot igrala v njem, bi v tabeli imeli dva vnosa.
