import itertools
import png


def nalozi_sliko(ime_datoteke):
    reader = png.Reader(filename=ime_datoteke)
    sirina, visina, rows, _ = reader.asRGBA()
    piksli = [[None for _ in range(sirina)] for _ in range(visina)]
    for vrstica, row in enumerate(rows):
        for stolpec, (r, g, b) in enumerate(zip(row[::4], row[1::4], row[2::4])):
            piksli[vrstica][stolpec] = (r, g, b)
    return piksli


def shrani_sliko(slika, ime_datoteke):
    vrstice = [list(itertools.chain(*vrstica)) for vrstica in slika]
    with open(ime_datoteke, 'wb') as datoteka:
        png.Writer(len(slika[0]), len(slika),
                   greyscale=False).write(datoteka, vrstice)


def razdalja(piksel1, piksel2):
    r1, g1, b1 = piksel1
    r2, g2, b2 = piksel2
    return ((r1 - r2) ** 2 + (g1 - g2) ** 2 + (b1 - b2) ** 2) ** 0.5


def energije_vrstice(vrstica):
    sirina = len(vrstica)
    energije = [0] * sirina
    for i in range(sirina - 1):
        r = razdalja(vrstica[i], vrstica[i + 1])
        energije[i] += r
        energije[i + 1] += r
    return energije


def energije_slike(slika):
    return [energije_vrstice(vrstica) for vrstica in slika]


def poisci_siv(slika):
    energije = energije_slike(slika)
    siv = []
    for vrstica in energije:
        siv.append(vrstica.index(min(vrstica)))
    return siv


def pokazi_siv(slika, siv):
    nova_slika = []
    for vrstica, i in zip(slika, siv):
        nova_vrstica = vrstica[:]
        nova_vrstica[i] = (255, 0, 0)
        nova_slika.append(nova_vrstica)
    return nova_slika


def odstrani_siv(slika, siv):
    nova_slika = []
    for vrstica, i in zip(slika, siv):
        nova_slika.append(vrstica[:i] + vrstica[i + 1:])
    return nova_slika


slika = nalozi_sliko('slike/8-kraljic.png')
for i in range(len(slika)):
    siv = poisci_siv(slika)
    if i % 10 == 0:
        shrani_sliko(pokazi_siv(slika, siv), f'korak-{i}.png')
    slika = odstrani_siv(slika, siv)
