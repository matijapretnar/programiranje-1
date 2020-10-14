import json
import re
import requests

filmov_na_stran = 50
stevilo_filmov = 200

vzorec = (
    r'<a href="/title/tt'
    r'(?P<id>\d{7})'  # ID ima sedem števk
    r'/\?ref_=adv_li_tt"\n>'  # neka šara vmes med id-jem in naslovom
    r'(?P<naslov>.*?)'  # zajamemo naslov
    r'</a>'
    r'\s+'
    r'<span class="lister-item-year text-muted unbold">'
    r'(\([IVXLCDM]+\) )?'
    r'\((?P<leto>.*?)\)'
)
count = 0
filmi = []

for start in range(1, stevilo_filmov, filmov_na_stran):
    with open(f'filmi-{start}-{start + filmov_na_stran - 1}.html') as f:
        vsebina = f.read()
    for zadetek in re.finditer(vzorec, vsebina):
        filmi.append(zadetek.groupdict())
        count += 1

with open('filmi.json', 'w') as f:
    json.dump(filmi, f, indent=2, ensure_ascii=True)
