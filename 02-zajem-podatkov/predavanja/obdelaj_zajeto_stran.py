import csv
import json
import re

with open('zajeti-podatki/50-najbolj-znanih-filmov.html') as datoteka:
    vsebina = datoteka.read()

vzorec = re.compile(
    r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
    r'>(?P<naslov>.+?)</a>.*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>\d+?) min</.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)<.*?',
    re.DOTALL
)


def pocisti_podatke(podatki):
    podatki['opis'] = podatki['opis'].strip()
    podatki['leto'] = int(podatki['leto'])
    podatki['dolzina'] = int(podatki['dolzina'])
    podatki['ocena'] = float(podatki['ocena'].replace(',', '.'))
    return podatki


podatki_filmov = []
with open('obdelani-podatki/prvih-50-filmov.csv', 'w') as csv_datoteka:
    writer = csv.DictWriter(
        csv_datoteka, ['id', 'naslov', 'dolzina', 'leto', 'ocena', 'opis'])
    writer.writeheader()
    for ujemanje in vzorec.finditer(vsebina):
        podatki_filma = pocisti_podatke(ujemanje.groupdict())
        writer.writerow(podatki_filma)
        podatki_filmov.append(podatki_filma)
with open('obdelani-podatki/prvih-50-filmov.json', 'w') as json_datoteka:
    json.dump(podatki_filmov, json_datoteka, indent=4, ensure_ascii=False)
