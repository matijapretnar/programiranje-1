import csv
import re
import json
import requests

vzorec_bloka = re.compile(
    r'<div class="lister-item mode-advanced">.*?'
    r'</p>\s*</div>\s*</div>',
    flags=re.DOTALL
)

vzorec_filma = re.compile(
    r'<a href="/title/tt(?P<id>\d+)/.*?".*?'
    r'img alt="(?P<naslov>.+?)".*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>\d+?) min</.*?'
    r'<span class="genre">(?P<zanri>.*?)</span>.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)</p.*?'
    r'Directors?:(?P<reziserji>.+?)(<span class="ghost">|</p>).*?'
    r'Votes:.*?data-value="(?P<glasovi>\d+)"',
    flags=re.DOTALL
)

vzorec_osebe = re.compile(
    r'<a\s+href="/name/nm(?P<id>\d+)/?[^>]*?>(?P<ime>.+?)</a>',
    flags=re.DOTALL
)

vzorec_povezave = re.compile(
    r'<a.*?>(.+?)</a>',
    flags=re.DOTALL
)

vzorec_zasluzka = re.compile(
    r'Gross:.*?data-value="(?P<zasluzek>(\d|,)+)"',
    flags=re.DOTALL
)

vzorec_metascore = re.compile(
    r'<span class="metascore.*?">(?P<metascore>\d+)',
    flags=re.DOTALL
)

vzorec_oznake = re.compile(
    r'<span class="certificate">(?P<oznaka>.+?)</span>',
    flags=re.DOTALL
)

vzorec_daljsi_povzetek = re.compile(
    r'<a href="/title/tt\d+/plotsummary.*?&nbsp;&raquo;',
    flags=re.DOTALL
)

vzorec_igralcev = re.compile(
    r'Stars?:(?P<igralci>.+?)</p>.*?',
    flags=re.DOTALL
)


def izloci_osebe(niz):
    osebe = []
    for oseba in vzorec_osebe.finditer(niz):
        osebe.append({
            'id': int(oseba.groupdict()['id']),
            'ime': oseba.groupdict()['ime'],
        })
    return osebe


def izloci_podatke_filma(blok):
    film = vzorec_filma.search(blok).groupdict()
    film['id'] = int(film['id'])
    film['dolzina'] = int(film['dolzina'])
    film['zanri'] = film['zanri'].strip().split(', ')
    film['leto'] = int(film['leto'])
    # odstranimo morebitno povezavo na daljši posnetek
    film['opis'] = vzorec_daljsi_povzetek.sub('', film['opis'])
    # odstranimo morebitne povezave v opisu
    film['opis'] = vzorec_povezave.sub(r'\1', film['opis'])
    film['opis'] = film['opis'].strip()
    film['ocena'] = float(film['ocena'])
    film['glasovi'] = int(film['glasovi'])
    film['reziserji'] = izloci_osebe(film['reziserji'])
    # zabeležimo oznako, če je omenjena
    oznaka = vzorec_oznake.search(blok)
    if oznaka:
        film['oznaka'] = oznaka['oznaka']
    else:
        film['oznaka'] = None
    # zabeležimo igralce, če so omenjeni
    igralci = vzorec_igralcev.search(blok)
    if igralci:
        film['igralci'] = izloci_osebe(igralci['igralci'])
    else:
        film['igralci'] = []
    # zabeležimo zaslužek, če je omenjen
    zasluzek = vzorec_zasluzka.search(blok)
    if zasluzek:
        film['zasluzek'] = int(zasluzek['zasluzek'].replace(',', ''))
    else:
        film['zasluzek'] = None
    # zabeležimo metascore, če je omenjen
    metascore = vzorec_metascore.search(blok)
    if metascore:
        film['metascore'] = int(metascore['metascore'])
    else:
        film['metascore'] = None
    return film

count = 0

def ime_datoteke(st_strani):
    return f"najboljsi-filmi-{st_strani}.html"

# for st_strani in range(20):
#     url = (
#         'https://www.imdb.com/search/title/'
#         '?title_type=feature&sort=num_votes,desc&count=250'
#         f'&start={250 * st_strani + 1}&ref_=adv_nxt'
#     )
#     print(f"Zajemam {url}")
#     response = requests.get(url, headers={
#         # "Accept-Language": "sl-si"
#     })
#     vsebina = response.text
#     with open(ime_datoteke(st_strani), 'w') as dat:
#         dat.write(vsebina)

filmi = []

for st_strani in range(20):
    with open(ime_datoteke(st_strani)) as dat:
        vsebina = dat.read()
    for blok in vzorec_bloka.finditer(vsebina):
        filmi.append(izloci_podatke_filma(blok.group(0)))

with open("filmi.json", "w") as dat:
    json.dump(filmi, dat, indent=4, ensure_ascii=False)

with open("filmi.csv", "w") as dat:
    writer = csv.DictWriter(dat, [
        "id",
        "naslov",
        "leto",
        "zasluzek",
        "glasovi",
        "dolzina",
        "metascore",
        "oznaka",
        "opis",
        "ocena",
        "reziserji",
        "igralci",
        "zanri",
    ])
    writer.writeheader()
    writer.writerows(filmi)
