import csv
import re
from utils import datoteke, ujemanja, shrani


shrani('http://www.imdb.com/chart/top?ref_=nv_mv_250_6', 'podatki/imdb_top.html')

regex_url_kategorije = re.compile(
    r'href="(?P<url>/search/title\?genres=(?P<kategorija>.+?)&sort=user_rating,desc&title_type=feature&num_votes=25000,)'
)

for ujemanje in ujemanja(regex_url_kategorije, 'podatki/imdb_top.html'):
    url = 'http://www.imdb.com{}'.format(ujemanje.group('url'))
    ime_datoteke = 'podatki/imdb/{}.html'.format(ujemanje.group('kategorija'))
    shrani(url, ime_datoteke)

regex_filma = re.compile(
    r'<tr class="(odd|even) detailed">.*?'
    r'<td class="number">(?P<rang>\d+)\.</td>.*?'
    r'title="(?P<naslov>.*?) \((?P<leto>\d{4})\)".*?'
    r'title="Users rated this (?P<ocena>.+?)/1',
    flags=re.DOTALL | re.MULTILINE
)

for html_datoteka in datoteke('podatki/imdb/'):
    if html_datoteka[-4:] == '.csv':
        continue
    csv_datoteka = html_datoteka.replace('.html', '.csv')
    imena_polj = ['naslov', 'leto', 'ocena', 'rang']
    with open(csv_datoteka, 'w') as csv_dat:
        writer = csv.DictWriter(csv_dat, fieldnames=imena_polj)
        writer.writeheader()
        for ujemanje in ujemanja(regex_filma, html_datoteka):
            writer.writerow(ujemanje.groupdict())
