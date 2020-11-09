import re
import orodja

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


def filmi_na_strani(st_strani, na_stran=250):
    url = (
        f'https://www.imdb.com/search/title'
        f'?sort=num_votes,desc&title_type=feature&count={na_stran}'
        f'&start={(st_strani - 1) * na_stran + 1}'
    )
    ime_datoteke = 'zajeti-podatki/najbolj-znani-filmi-{}.html'.format(st_strani)
    orodja.shrani_spletno_stran(url, ime_datoteke)
    vsebina = orodja.vsebina_datoteke(ime_datoteke)
    for blok in vzorec_bloka.finditer(vsebina):
        yield izloci_podatke_filma(blok.group(0))


def izloci_gnezdene_podatke(filmi):
    REZISER, IGRALEC = 'R', 'I'
    osebe, vloge, zanri = [], [], []
    videne_osebe = set()

    def dodaj_vlogo(film, oseba, vloga, mesto):
        if oseba['id'] not in videne_osebe:
            videne_osebe.add(oseba['id'])
            osebe.append(oseba)
        vloge.append({
            'film': film['id'],
            'oseba': oseba['id'],
            'vloga': vloga,
            'mesto': mesto,
        })


    for film in filmi:
        for zanr in film.pop('zanri'):
            zanri.append({'film': film['id'], 'zanr': zanr})
        for mesto, oseba in enumerate(film.pop('reziserji'), 1):
            dodaj_vlogo(film, oseba, REZISER, mesto)
        for mesto, oseba in enumerate(film.pop('igralci'), 1):
            dodaj_vlogo(film, oseba, IGRALEC, mesto)

    osebe.sort(key=lambda oseba: oseba['id'])
    vloge.sort(key=lambda vloga: (vloga['film'], vloga['vloga'], vloga['mesto']))
    zanri.sort(key=lambda zanr: (zanr['film'], zanr['zanr']))

    return osebe, vloge, zanri


filmi = []
for st_strani in range(1, 41):
    for film in filmi_na_strani(st_strani, 250):
        filmi.append(film)
filmi.sort(key=lambda film: film['id'])
orodja.zapisi_json(filmi, 'obdelani-podatki/filmi.json')
osebe, vloge, zanri = izloci_gnezdene_podatke(filmi)
orodja.zapisi_csv(
    filmi,
    ['id', 'naslov', 'dolzina', 'leto', 'ocena', 'metascore', 'glasovi', 'zasluzek', 'oznaka', 'opis'], 'obdelani-podatki/filmi.csv'
)
orodja.zapisi_csv(osebe, ['id', 'ime'], 'obdelani-podatki/osebe.csv')
orodja.zapisi_csv(vloge, ['film', 'oseba', 'vloga', 'mesto'], 'obdelani-podatki/vloge.csv')
orodja.zapisi_csv(zanri, ['film', 'zanr'], 'obdelani-podatki/zanri.csv')
