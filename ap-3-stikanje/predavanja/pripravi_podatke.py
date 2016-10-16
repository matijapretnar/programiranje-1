import re
import orodja


regex_filma = re.compile(
    r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
    r'>(?P<naslov>.+?)</a>.*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>\d+?) min</.*?'
    r'<span class="genre">(?P<zanri>.*?)</span>.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)<.*?'
    r'Directors?:(?P<reziserji>.+?)<span class="ghost">.*?'
    r'Stars:(?P<igralci>.+?)</p>',
    flags=re.DOTALL
)


regex_osebe = re.compile(
    r'href="/name/nm(?P<id>\d+)/[^>]*?>(?P<ime>.+?)</a>',
    flags=re.DOTALL
)


def zajemi_spletne_strani():
    osnovni_naslov = 'http://www.imdb.com/search/title'
    parametri = 'sort=num_votes,desc&title_type=feature&num_votes=25000,'
    for stran in range(1, 51):
        naslov = '{}?{}&page={}'.format(osnovni_naslov, parametri, stran)
        ime_datoteke = 'imdb/{:02}.html'.format(stran)
        orodja.shrani(naslov, ime_datoteke)


def izloci_osebe(niz):
    return [match.groupdict() for match in re.finditer(regex_osebe, niz)]


def pocisti_film(film):
    podatki = film.groupdict()
    podatki['id'] = int(podatki['id'])
    podatki['dolzina'] = int(podatki['dolzina'])
    podatki['zanri'] = podatki['zanri'].strip().split(', ')
    podatki['leto'] = int(podatki['leto'])
    podatki['opis'] = podatki['opis'].strip()
    podatki['ocena'] = float(podatki['ocena'])
    podatki['reziserji'] = izloci_osebe(podatki['reziserji'])
    podatki['igralci'] = izloci_osebe(podatki['igralci'])
    return podatki


def izloci_podatke_filmov():
    filmi = []
    for html_datoteka in orodja.datoteke('imdb/'):
        for film in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
            filmi.append(pocisti_film(film))
    return filmi


def razdeli_tabelo(filmi):
    osebe, vloge, zanri = [], [], []
    videne_osebe = set()

    for film in filmi:
        for zanr in film.pop('zanri'):
            zanri.append({'film': film['id'], 'zanr': zanr})
        for reziser in film.pop('reziserji'):
            if reziser['id'] not in videne_osebe:
                videne_osebe.add(reziser['id'])
                osebe.append(reziser)
            vloge.append({'film': film['id'], 'oseba': reziser['id'], 'vloga': 'reziser'})
        for igralec in film.pop('igralci'):
            if igralec['id'] not in videne_osebe:
                videne_osebe.add(igralec['id'])
                osebe.append(igralec)
            vloge.append({'film': film['id'], 'oseba': igralec['id'], 'vloga': 'igralec'})

    return filmi, osebe, vloge, zanri


zajemi_spletne_strani()
filmi = izloci_podatke_filmov()
filmi, osebe, vloge, zanri = razdeli_tabelo(filmi)
orodja.zapisi_tabelo(filmi, ['id', 'naslov', 'leto', 'dolzina', 'ocena', 'opis'], 'filmi.csv')
orodja.zapisi_tabelo(osebe, ['id', 'ime'], 'osebe.csv')
orodja.zapisi_tabelo(vloge, ['film', 'oseba', 'vloga'], 'vloge.csv')
orodja.zapisi_tabelo(zanri, ['film', 'zanr'], 'zanri.csv')
