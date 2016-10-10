import re
import orodja


def zajemi_imdb():
    osnovni_naslov = 'http://www.imdb.com/search/title'
    parametri = 'sort=num_votes,desc&title_type=feature&num_votes=25000,'
    for stran in range(1, 51):
        naslov = '{}?{}&page={}'.format(osnovni_naslov, parametri, stran)
        ime_datoteke = 'imdb/{:02}.html'.format(stran)
        orodja.shrani(naslov, ime_datoteke)


def pocisti_film(film):
    podatki = film.groupdict()
    podatki['id'] = int(podatki['id'])
    podatki['runtime'] = int(podatki['runtime'])
    podatki['leto'] = int(podatki['leto'])
    podatki['opis'] = podatki['opis'].strip()
    podatki['ocena'] = float(podatki['ocena'])
    return podatki


def pripravi_imdb():
    regex_filma = re.compile(
        r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
        r'>(?P<naslov>.*?)</a>.*?'
        r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
        r'(certificate">(?P<certifikat>.*?)</.*?)?'
        r'runtime">(?P<runtime>.*?) min</.*?'
        r'<strong>(?P<ocena>.+?)</strong>.*?'
        r'<p class="text-muted">(?P<opis>.+?)<.*?'
        r'Directors?:.*?<a href=.*?>(?P<reziser>.+?)</a>.*?',
        flags=re.DOTALL
    )

    filmi = []
    for html_datoteka in orodja.datoteke('imdb/'):
        for film in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
            filmi.append(pocisti_film(film))

    orodja.zapisi_tabelo(filmi, ['id', 'naslov', 'leto', 'reziser', 'certifikat', 'runtime', 'ocena', 'opis'], 'filmi.csv')


zajemi_imdb()
pripravi_imdb()
