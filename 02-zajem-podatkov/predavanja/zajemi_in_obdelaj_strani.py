import csv
import json
import os
import re
import sys

import requests


def pripravi_imenik(ime_datoteke):
    '''Če še ne obstaja, pripravi prazen imenik za dano datoteko.'''
    imenik = os.path.dirname(ime_datoteke)
    if imenik:
        os.makedirs(imenik, exist_ok=True)


def shrani_spletno_stran(url, ime_datoteke, vsili_prenos=False):
    '''Vsebino strani na danem naslovu shrani v datoteko z danim imenom.'''
    try:
        print('Shranjujem {} ...'.format(url), end='')
        sys.stdout.flush()
        if os.path.isfile(ime_datoteke) and not vsili_prenos:
            print('shranjeno že od prej!')
            return
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        print('stran ne obstaja!')
    else:
        pripravi_imenik(ime_datoteke)
        with open(ime_datoteke, 'w', encoding='utf-8') as datoteka:
            datoteka.write(r.text)
            print('shranjeno!')


def vsebina_datoteke(ime_datoteke):
    '''Vrne niz z vsebino datoteke z danim imenom.'''
    with open(ime_datoteke, encoding='utf-8') as datoteka:
        return datoteka.read()


def zapisi_csv(slovarji, imena_polj, ime_datoteke):
    '''Iz seznama slovarjev ustvari CSV datoteko z glavo.'''
    pripravi_imenik(ime_datoteke)
    with open(ime_datoteke, 'w', encoding='utf-8') as csv_datoteka:
        writer = csv.DictWriter(csv_datoteka, fieldnames=imena_polj)
        writer.writeheader()
        for slovar in slovarji:
            writer.writerow(slovar)


def zapisi_json(objekt, ime_datoteke):
    '''Iz danega objekta ustvari JSON datoteko.'''
    pripravi_imenik(ime_datoteke)
    with open(ime_datoteke, 'w', encoding='utf-8') as json_datoteka:
        json.dump(objekt, json_datoteka, indent=4, ensure_ascii=False)


vzorec = re.compile(
    r'href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt"[^>]*?'
    r'>(?P<naslov>.+?)</a>.*?'
    r'lister-item-year text-muted unbold">.*?\((?P<leto>\d{4})\)</span>.*?'
    r'runtime">(?P<dolzina>\d+?) min</.*?'
    r'<strong>(?P<ocena>.+?)</strong>.*?'
    r'<p class="text-muted">(?P<opis>.+?)<.*?',
    re.DOTALL
)


def izloci_podatke_filma(ujemanje_filma):
    podatki_filma = ujemanje_filma.groupdict()
    podatki_filma['opis'] = podatki_filma['opis'].strip()
    podatki_filma['leto'] = int(podatki_filma['leto'])
    podatki_filma['dolzina'] = int(podatki_filma['dolzina'])
    podatki_filma['ocena'] = float(podatki_filma['ocena'].replace(',', '.'))
    return podatki_filma


for i in range(1, 11):
    url = (
        'https://www.imdb.com/search/title'
        '?sort=num_votes,desc&title_type=feature&count=250'
        '&page={}'
    ).format(i)
    shrani_spletno_stran(url, 'zajeti-podatki/najbolj-znani-filmi-{}.html'.format(i))


podatki_filmov = []
for i in range(1, 11):
    vsebina = vsebina_datoteke(
        'zajeti-podatki/najbolj-znani-filmi-{}.html'.format(i))
    for ujemanje_filma in vzorec.finditer(vsebina):
        podatki_filmov.append(izloci_podatke_filma(ujemanje_filma))
zapisi_json(podatki_filmov, 'obdelani-podatki/vsi-filmi.json')
zapisi_csv(podatki_filmov, ['id', 'naslov', 'dolzina', 'leto',
                            'ocena', 'opis'], 'obdelani-podatki/vsi-filmi.csv')
