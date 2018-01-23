import csv
import json
import os
import re
import requests


re_bloka_serije = re.compile(
    r'<a href="/title/tt(?P<id>\d+).*?Votes.*?</div>', flags=re.DOTALL
)

re_podatkov_serije = re.compile(
    # ID in ime serije
    r'<a href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"\n>(?P<ime>.*?)</a>'
    r'.*?'
    # leto serije
    r'<span class="lister-item-year text-muted unbold">'
    r'(\((?P<oznaka>[IVXLCDM]+)\) )?'  # morebitna oznaka za več serij z istim imenom
    r'\('
    r'(?P<leto_zacetka>\d{4})'
    r'(?P<vezaj_leta>–? ?)'
    r'(?P<leto_konca>\d{4})?'
    r'\)</span>'
    r'.*?'
    # različni podatki
    r'<p class="text-muted ">(?P<podatki>.*?)</p>'
    r'.*?'
    # ocena
    r'<meta itemprop="ratingValue" content="(?P<ocena>\d(\.\d)?)" />'
    r'.*?'
    # opis
    r'<p class="text-muted">(?P<opis>.*?)</p>'
    r'.*?'
    # igralci
    r'Stars?:(?P<igralci>.*?)</p>'
    r'.*?'
    # število glasov
    r'<span name="nv" data-value="(?P<stevilo_glasov>\d+)">',
    flags=re.DOTALL
)

re_dolzine = re.compile(
    r'<span class="runtime">(?P<dolzina>\d+) min</span>'
)

re_zanrov = re.compile(
    r'<span class="genre">(?P<zanri>.*?)</span>',
    flags=re.DOTALL
)

re_igralca = re.compile(
    r'<a href="/name/nm(?P<id>\d+)/\?ref_=adv_li_st_\d+"\n>'
    r'(?P<ime>.*?)'
    r'</a>'
)


def podatki_serije(blok_serije):
    ujemanje = re_podatkov_serije.search(blok_serije)
    if ujemanje:
        serija = ujemanje.groupdict()
        serija['id'] = int(serija['id'])
        serija['igralci'] = [
            ujemanje_igralca.groupdict() for ujemanje_igralca in re_igralca.finditer(serija['igralci'])
        ]
        serija['opis'] = serija['opis'].strip()
        ujemanje_dolzine = re_dolzine.search(serija['podatki'])
        serija['dolzina'] = ujemanje_dolzine.group('dolzina') if ujemanje_dolzine else None
        ujemanje_zanrov = re_zanrov.search(serija['podatki'])
        serija['zanri'] = ujemanje_zanrov.group('zanri').strip().split(', ') if ujemanje_zanrov else []
        if not serija.pop('vezaj_leta') and not serija['leto_konca']:
            serija['leto_konca'] = serija['leto_zacetka']
        del serija['podatki']
        return serija
    else:
        print('ENE SERIJE PA NE ZNAM PREBRATI')
        print(blok_serije)


def shrani_serije_v_imenik(imenik, stevilo_strani=20, stevilo_serij_na_stran=100):
    os.makedirs(imenik, exist_ok=True)
    for stevilka_strani in range(1, stevilo_strani + 1):
        naslov_strani = (
            'http://www.imdb.com/search/title?'
            'sort=num_votes,desc&title_type=tv_series&'
            'page={}&count={}'
        ).format(stevilka_strani, stevilo_serij_na_stran)
        stran = requests.get(naslov_strani)
        ime_datoteke = 'stran-{}.html'.format(stevilka_strani)
        polna_pot_datoteke = os.path.join(imenik, ime_datoteke)
        with open(polna_pot_datoteke, 'w', encoding='utf-8') as datoteka:
            datoteka.write(stran.text)


def preberi_serije_v_imeniku(imenik):
    serije = []
    for ime_datoteke in os.listdir(imenik):
        polna_pot_datoteke = os.path.join(imenik, ime_datoteke)
        with open(polna_pot_datoteke) as datoteka:
            vsebina_datoteke = datoteka.read()
            for blok_serije in re_bloka_serije.finditer(vsebina_datoteke):
                serije.append(podatki_serije(blok_serije.group(0)))
    return serije


def zapisi_json(podatki, ime_datoteke):
    with open(ime_datoteke, 'w') as datoteka:
        json.dump(podatki, datoteka, indent=2)


def zapisi_csv(podatki, polja, ime_datoteke):
    with open(ime_datoteke, 'w') as datoteka:
        pisalec = csv.DictWriter(datoteka, polja, extrasaction='ignore')
        pisalec.writeheader()
        for podatek in podatki:
            pisalec.writerow(podatek)


# shrani_serije_v_imenik('serije')
serije = preberi_serije_v_imeniku('serije')
zanri = [
    {'serija': serija['id'], 'zanr': zanr}
    for serija in serije for zanr in serija['zanri']
]
igralci = {
    igralec['id']: igralec
    for serija in serije for igralec in serija['igralci']
}.values()
vloge = [
    {'serija': serija['id'], 'igralec': igralec['id']}
    for serija in serije for igralec in serija['igralci']
]

zapisi_json(serije, 'serije.json')
polja = [
    'id', 'ime', 'oznaka', 'leto_zacetka', 'leto_konca', 'ocena',
    'opis', 'stevilo_glasov', 'dolzina',
]
zapisi_csv(serije, polja, 'serije.csv')
zapisi_csv(zanri, ['serija', 'zanr'], 'zanri.csv')
zapisi_csv(igralci, ['id', 'ime'], 'igralci.csv')
zapisi_csv(vloge, ['serija', 'igralec'], 'vloge.csv')