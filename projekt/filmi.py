import csv
import re
import orodja


def zajemi_imdb():
    orodja.shrani('http://www.imdb.com/chart/top?ref_=nv_mv_250_6', 'zajeti-podatki/imdb.html')

    regex_url_kategorije = re.compile(
        r'href="(?P<url>/search/title\?genres=(?P<kategorija>.+?)'
        r'&sort=user_rating,desc&title_type=feature&num_votes=25000,)'
    )

    for ujemanje in re.finditer(regex_url_kategorije,
                                orodja.vsebina_datoteke('zajeti-podatki/imdb.html')):
        url = 'http://www.imdb.com{}'.format(ujemanje.group('url'))
        ime_datoteke = 'zajeti-podatki/imdb/{}.html'.format(ujemanje.group('kategorija'))
        orodja.shrani(url, ime_datoteke)


def zajemi_rotten():
    orodja.shrani('http://www.rottentomatoes.com/top/', 'zajeti-podatki/rotten.html')

    regex_url_kategorije = re.compile(
        r'href="(?P<url>/top/bestofrt/top_100_(?P<kategorija>.+?)_movies/)"'
    )

    for ujemanje in re.finditer(regex_url_kategorije,
                                orodja.vsebina_datoteke('zajeti-podatki/rotten.html')):
        url = 'http://www.rottentomatoes.com{}'.format(ujemanje.group('url'))
        ime_datoteke = 'zajeti-podatki/rotten/{}.html'.format(ujemanje.group('kategorija'))
        orodja.shrani(url, ime_datoteke)


def pripravi_imdb():
    regex_filma = re.compile(
        r'<tr class="(odd|even) detailed">.*?'
        r'<td class="number">(?P<rang>\d+)\.</td>.*?'
        r'title="(?P<naslov>.*?) \((?P<leto>\d{4})\)".*?'
        r'title="Users rated this (?P<ocena>.+?)/1(0|1)',
        flags=re.DOTALL
    )

    for html_datoteka in orodja.datoteke('zajeti-podatki/imdb/'):
        csv_datoteka = html_datoteka.replace('.html', '.csv').replace('zajeti', 'pripravljeni')
        imena_polj = ['naslov', 'leto', 'ocena', 'rang']
        orodja.pripravi_imenik(csv_datoteka)
        with open(csv_datoteka, 'w') as csv_dat:
            writer = csv.DictWriter(csv_dat, fieldnames=imena_polj)
            writer.writeheader()
            for ujemanje in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
                writer.writerow(ujemanje.groupdict())


def pripravi_rotten():
    regex_filma = re.compile(
        r'<tr> <td class="bold">(?P<rang>\d+).</td>.*?'
        r'<span class="tMeterScore">(?P<ocena>\d+)%</span>.*?'
        r'<a .*?>(?P<naslov>.*?) \((?P<leto>\d{4})\)</a>'
    )

    for html_datoteka in orodja.datoteke('zajeti-podatki/rotten/'):
        csv_datoteka = html_datoteka.replace('.html', '.csv').replace('zajeti', 'pripravljeni')
        imena_polj = ['naslov', 'leto', 'ocena', 'rang']
        orodja.pripravi_imenik(csv_datoteka)
        with open(csv_datoteka, 'w') as csv_dat:
            writer = csv.DictWriter(csv_dat, fieldnames=imena_polj)
            writer.writeheader()
            for ujemanje in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
                writer.writerow(ujemanje.groupdict())


zajemi_imdb()
zajemi_rotten()
pripravi_imdb()
pripravi_rotten()
