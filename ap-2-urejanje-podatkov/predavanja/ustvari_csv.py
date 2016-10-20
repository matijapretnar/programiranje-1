import re
import orodja


regex_filma = re.compile(
    r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
    r'>(?P<naslov>.*?)</a>.*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>.*?) min</.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)<.*?'
    r'Directors?:.*?<a href=.*?>(?P<reziser>.+?)</a>.*?',
    flags=re.DOTALL
)


def pocisti_film(film):
    podatki = film.groupdict()
    podatki['id'] = int(podatki['id'])
    podatki['dolzina'] = int(podatki['dolzina'])
    podatki['leto'] = int(podatki['leto'])
    podatki['opis'] = podatki['opis'].strip()
    podatki['ocena'] = float(podatki['ocena'])
    return podatki


def izloci_podatke_filmov(imenik):
    filmi = []
    for html_datoteka in orodja.datoteke(imenik):
        for film in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
            filmi.append(pocisti_film(film))
    return filmi


filmi = izloci_podatke_filmov('../../ap-1-zajem-podatkov/predavanja/imdb/')
orodja.zapisi_tabelo(filmi, ['id', 'naslov', 'leto', 'reziser', 'dolzina', 'ocena', 'opis'], 'filmi.csv')
