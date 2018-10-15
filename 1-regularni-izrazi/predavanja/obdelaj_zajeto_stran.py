import csv
import re

with open('zajeti-podatki/50-najbolj-znanih-filmov.html') as datoteka:
    vsebina = datoteka.read()

vzorec = '<a href="/title/tt(\\d+)/\\?ref_=adv_li_tt"\n>(.*?)</a>'

# CSV datoteke raje ne delamo na roke, ker moramo paziti na kup robnih primerov,
# na primer filme, ki v naslovu vsebujejo vejico.
with open('obdelani-podatki/slabo-narejeni-filmi.csv', 'w') as datoteka:
    print('id,naslov', file=datoteka)
    for ujemanje in re.finditer(vzorec, vsebina, re.DOTALL):
        print('{},{}'.format(ujemanje.group(1), ujemanje.group(2)), file=datoteka)

# Bolje je uporabiti knjižnico csv, ki poskrbi za vse.
with open('obdelani-podatki/pravilno-narejeni-filmi.csv', 'w') as datoteka:
    writer = csv.writer(datoteka)
    writer.writerow(('id', 'naslov'))
    for ujemanje in re.finditer(vzorec, vsebina, re.DOTALL):
        writer.writerow((ujemanje.group(1), ujemanje.group(2)))
