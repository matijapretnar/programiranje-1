import bs4, re

with open('250-najbolj-znanih-filmov.html') as f:
    vsebina = f.read()

zupa = bs4.BeautifulSoup(vsebina, 'html.parser')

count = 0
for povezava in zupa.find_all('a'):
    link = povezava.get('href')
    if link and link.endswith('?ref_=adv_li_tt'):
        naslov = povezava.string
        id = int(re.search('\d{7}', link).group(0))
        znacka_z_letom = povezava.find_next_sibling()
        niz_z_letom = znacka_z_letom.string
        vzorec_leta = r'(\([IVXLCDM]+\) )?\((?P<leto>.*?)\)'
        leto = int(re.fullmatch(vzorec_leta, niz_z_letom).group('leto'))
        print({'id': id, 'naslov': naslov, 'leto': leto})
        count += 1
print(count)
