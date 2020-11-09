import orodja
import re
import requests

STEVILO_STRANI = 10
STEVILO_FILMOV_NA_STRAN = 50

vzorec = (
    r'<a href="/title/tt'
    r'(?P<id>\d{7})'  # ID ima sedem števk
    r'/.*?"\n>'  # neka šara vmes med id-jem in naslovom
    r'(?P<naslov>.*?)'  # zajamemo naslov
    r'</a>'
    r'\s+'
    r'<span class="lister-item-year text-muted unbold">'
    r'(\([IVXLCDM]+\) )?'
    r'\((?P<leto>.*?)\)'
)

najdeni_filmi = 0

for stran in range(STEVILO_STRANI):
    count = STEVILO_FILMOV_NA_STRAN
    start = 1 + stran * STEVILO_FILMOV_NA_STRAN
    url = f'https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count={count}&start={start}'
    datoteka = f'najbolj-znani-filmi/{start}-{start + count - 1}.html' 
    orodja.shrani_spletno_stran(url, datoteka)
    vsebina = orodja.vsebina_datoteke(datoteka)

    for zadetek in re.finditer(vzorec, vsebina):
        # print(zadetek.groupdict())
        najdeni_filmi += 1

print(najdeni_filmi)