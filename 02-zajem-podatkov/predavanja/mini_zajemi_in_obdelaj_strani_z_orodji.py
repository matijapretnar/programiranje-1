import json
import re
import requests
import orodja

filmov_na_stran = 50
stevilo_filmov = 200

vzorec = (
    r'<a href="/title/tt'
    r'(?P<id>\d{7})'  # ID ima sedem števk
    r'/(\?ref_=adv_li_tt)?"\n>'  # neka šara vmes med id-jem in naslovom
    r'(?P<naslov>.*?)'  # zajamemo naslov
    r'</a>'
    r'\s+'
    r'<span class="lister-item-year text-muted unbold">\('
    r'(?P<leto>\d{4})'
    r'\)'
)
count = 0
filmi = []

for start in range(1, stevilo_filmov, filmov_na_stran):
    url = (
        'https://www.imdb.com/search/title/?'
        'title_type=feature&sort=num_votes,desc&'
        f'count={filmov_na_stran}&start={start}'
    )
    ime_datoteke = f'filmi-{start}-{start + filmov_na_stran - 1}.html'
    orodja.shrani_spletno_stran(url, ime_datoteke)
    vsebina = orodja.vsebina_datoteke(ime_datoteke)
    for zadetek in re.finditer(vzorec, vsebina):
        filmi.append(zadetek.groupdict())
        count += 1

orodja.zapisi_json(filmi, 'filmi.json')
