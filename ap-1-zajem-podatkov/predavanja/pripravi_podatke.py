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
    r'(\((?P<oznaka>I+)\) )?'  # morebitna oznaka za več serij z istim imenom
    r'\('
    r'(?P<leto_zacetka>\d{4})'
    r'–? ?'
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
    serija = re_podatkov_serije.search(blok_serije).groupdict()
    serija['igralci'] = [
        ujemanje_igralca.groupdict() for ujemanje_igralca in re_igralca.finditer(serija['igralci'])
    ]
    serija['opis'] = serija['opis'].strip()
    ujemanje_dolzine = re_dolzine.search(serija['podatki'])
    serija['dolzina'] = ujemanje_dolzine.group('dolzina') if ujemanje_dolzine else None
    ujemanje_zanrov = re_zanrov.search(serija['podatki'])
    serija['zanri'] = ujemanje_zanrov.group('zanri').strip().split(', ') if ujemanje_zanrov else []
    del serija['podatki']
    return serija


def shrani_serije_v_imenik(imenik, stevilo_strani=20, stevilo_serij_na_stran=100):
    os.makedirs(imenik, exist_ok=True)
    for stevilka_strani in range(stevilo_strani):
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
        datoteka = open(polna_pot_datoteke)
        vsebina_datoteke = datoteka.read()
        for blok_serije in re_bloka_serije.finditer(vsebina_datoteke):
            serije.append(podatki_serije(blok_serije.group(0)))
    return serije


shrani_serije_v_imenik('testni-imenik-2', 1, 10)
# shrani_serije_v_imenik('serije')
serije = preberi_serije_v_imeniku('serije')