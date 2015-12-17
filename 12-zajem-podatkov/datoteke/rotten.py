import csv
import re
from utils import datoteke, ujemanja, shrani


shrani('http://www.rottentomatoes.com/top/', 'podatki/rotten.html')

regex_url_kategorije = re.compile(
    r'href="(?P<url>/top/bestofrt/top_100_(?P<kategorija>.+?)_movies/)"'
)

for ujemanje in ujemanja(regex_url_kategorije, 'podatki/rotten.html'):
    url = 'http://www.rottentomatoes.com{}'.format(ujemanje.group('url'))
    ime_datoteke = 'podatki/rotten/{}.html'.format(ujemanje.group('kategorija'))
    shrani(url, ime_datoteke)

regex_filma = re.compile(
    r'<tr> <td class="bold">(?P<rang>\d+).</td>.*?'
    r'<span class="tMeterScore">(?P<ocena>\d+)%</span>.*?'
    r'<a .*?>(?P<naslov>.*?) \((?P<leto>\d{4})\)</a>'
)

for html_datoteka in datoteke('podatki/rotten/'):
    if html_datoteka[-4:] == '.csv':
        continue
    csv_datoteka = html_datoteka.replace('.html', '.csv')
    imena_polj = ['naslov', 'leto', 'ocena', 'rang']
    with open(csv_datoteka, 'w') as csv_dat:
        writer = csv.DictWriter(csv_dat, fieldnames=imena_polj)
        writer.writeheader()
        for ujemanje in ujemanja(regex_filma, html_datoteka):
            writer.writerow(ujemanje.groupdict())
