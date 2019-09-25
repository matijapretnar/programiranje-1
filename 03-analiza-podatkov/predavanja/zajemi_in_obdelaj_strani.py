import re

import orodja

vzorec_bloka = re.compile(
    r'<a href="/title/tt\d+/\?ref_=adv_li_tt".*?'
    r'</p>\s*</div>\s*</div>',
    flags=re.DOTALL
)

vzorec_filma = re.compile(
    r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
    r'>(?P<naslov>.+?)</a>.*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>\d+?) min</.*?'
    r'<span class="genre">(?P<zanri>.*?)</span>.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)</p.*?'
    r'Directors?:(?P<reziserji>.+?)<span class="ghost">.*?'
    r'Stars?:(?P<igralci>.+?)</p>.*?'
    r'Votes:.*?data-value="(?P<glasovi>\d+)"',
    flags=re.DOTALL
)


vzorec_osebe = re.compile(
    r'<a\s+href="/name/nm(?P<id>\d+)/?[^>]*?>(?P<ime>.+?)</a>',
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

vzorec_daljsi_povzetek = re.compile(
    r'<a href="/title/tt\d+/plotsummary.*?&nbsp;&raquo;',
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
    # odstranimo morebitne povezave na osebe
    film['opis'] = vzorec_osebe.sub(r'\2', film['opis'])
    film['opis'] = film['opis'].strip()
    film['ocena'] = float(film['ocena'])
    film['glasovi'] = int(film['glasovi'])
    film['reziserji'] = izloci_osebe(film['reziserji'])
    film['igralci'] = izloci_osebe(film['igralci'])
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


def filmi_na_strani(st_strani):
    url = (
        'https://www.imdb.com/search/title'
        '?sort=num_votes,desc&title_type=feature&count=250'
        '&page={}'
    ).format(st_strani)
    ime_datoteke = 'zajeti-podatki/najbolj-znani-filmi-{}.html'.format(
        st_strani)
    orodja.shrani_spletno_stran(url, ime_datoteke)
    vsebina = orodja.vsebina_datoteke(ime_datoteke)
    for blok in vzorec_bloka.finditer(vsebina):
        yield izloci_podatke_filma(blok.group(0))


def izloci_gnezdene_podatke(filmi):
    osebe, vloge, zanri = [], [], []
    videne_osebe = set()

    for film in filmi:
        for zanr in film.pop('zanri'):
            zanri.append({'film': film['id'], 'zanr': zanr})
        for reziser in film.pop('reziserji'):
            if reziser['id'] not in videne_osebe:
                videne_osebe.add(reziser['id'])
                osebe.append(reziser)
            vloge.append(
                {'film': film['id'], 'oseba': reziser['id'], 'vloga': 'reziser'})
        for igralec in film.pop('igralci'):
            if igralec['id'] not in videne_osebe:
                videne_osebe.add(igralec['id'])
                osebe.append(igralec)
            vloge.append(
                {'film': film['id'], 'oseba': igralec['id'], 'vloga': 'igralec'})

    osebe.sort(key=lambda oseba: oseba['id'])
    vloge.sort(key=lambda vloga: (
        vloga['film'], vloga['oseba'], vloga['vloga']))
    zanri.sort(key=lambda zanr: (zanr['film'], zanr['zanr']))

    return osebe, vloge, zanri


filmi = []
for st_strani in range(1, 11):
    for film in filmi_na_strani(st_strani):
        filmi.append(film)
filmi.sort(key=lambda film: film['id'])
orodja.zapisi_json(filmi, 'obdelani-podatki/filmi.json')
osebe, vloge, zanri = izloci_gnezdene_podatke(filmi)
orodja.zapisi_csv(filmi, ['id', 'naslov', 'dolzina', 'leto', 'ocena',
                          'metascore', 'glasovi', 'zasluzek', 'opis'], 'obdelani-podatki/filmi.csv')
orodja.zapisi_csv(osebe, ['id', 'ime'], 'obdelani-podatki/osebe.csv')
orodja.zapisi_csv(vloge, ['film', 'oseba', 'vloga'],
                  'obdelani-podatki/vloge.csv')
orodja.zapisi_csv(zanri, ['film', 'zanr'], 'obdelani-podatki/zanri.csv')
