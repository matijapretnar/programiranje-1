import re

with open("podatki-o-filmih.html") as f:
    vsebina = f.read()

def rocno_poisci_vse_pojavitve(niz: str, vzorec):
    zacetek = 0
    while True:
        zacetek = niz.find(vzorec, zacetek + len(niz))
        if zacetek == -1:
            break
        yield zacetek

def poisci_vse_pojavitve(niz: str, vzorec):
    for m in re.finditer(vzorec, niz):
        yield m.start(), m.end()

def vse_pojavitve(niz, vzorec, kontekst=20):
    for zacetek, konec in poisci_vse_pojavitve(niz, vzorec):
        print(niz[zacetek - kontekst:konec + kontekst])
        print(kontekst * ' ' + (konec - zacetek) * '^')


count = 0
for film in re.finditer(r'<a href="/title/tt(?P<id>\d+)/\?ref_=adv_li_tt">(?P<naslov>.*)</a>', vsebina):
    count += 1
    print(film.groupdict())
print(count)