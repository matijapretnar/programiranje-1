import re
import orodja


def zajemi_imdb():
    orodja.shrani('http://www.imdb.com/chart/top?ref_=nv_mv_250_6', 'zajete-strani/imdb.html')

    regex_url_zanra = re.compile(
        r'href="(?P<url>/search/title\?genres=(?P<zanr>.+?)'
        r'&sort=user_rating,desc&title_type=feature&num_votes=25000,)'
    )

    for zanr in re.finditer(regex_url_zanra,
                            orodja.vsebina_datoteke('zajete-strani/imdb.html')):
        url = 'http://www.imdb.com{}'.format(zanr.group('url'))
        ime_datoteke = 'zajete-strani/imdb/{}.html'.format(zanr.group('zanr'))
        orodja.shrani(url, ime_datoteke)


def zajemi_rotten():
    orodja.shrani('http://www.rottentomatoes.com/top/', 'zajete-strani/rotten.html')

    regex_url_zanra = re.compile(
        r'href="(?P<url>/top/bestofrt/top_100_(?P<zanr>.+?)_movies/)"'
    )

    for zanr in re.finditer(regex_url_zanra,
                            orodja.vsebina_datoteke('zajete-strani/rotten.html')):
        url = 'http://www.rottentomatoes.com{}'.format(zanr.group('url'))
        ime_datoteke = 'zajete-strani/rotten/{}.html'.format(zanr.group('zanr'))
        orodja.shrani(url, ime_datoteke)


def pripravi_imdb():
    regex_filma = re.compile(
        r'<tr class="(odd|even) detailed">.*?'
        r'href="/title/tt(?P<id>\d+)/".*?'
        r'title="(?P<naslov>.*?) \((?P<leto>\d{4})\)".*?'
        r'title="Users rated this (?P<ocena>.+?)/1(0|1).*?'
        r'<span class="credit">.*?With: (?P<igralci>.*?)</span>.*?'
        r'<span class="genre">(?P<zanri>.*?)</span>',
        flags=re.DOTALL
    )

    filmi, igralci, zanri = {}, {}, {}
    vloge, dolocitve_zanra = set(), set()

    for html_datoteka in orodja.datoteke('zajete-strani/imdb/'):
        for film in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
            id_filma, podatki = uredi_film(film)
            # print(id_filma, podatki)
            igralci_filma = podatki.pop('igralci')
            zanri_filma = podatki.pop('zanri')
            filmi[id_filma] = podatki
            for id_igralca, ime_igralca in igralci_filma.items():
                igralci[id_igralca] = {'id': id_igralca, 'ime': ime_igralca}
                vloge.add((id_igralca, id_filma))
            for id_zanra, ime_zanra in zanri_filma.items():
                zanri[id_zanra] = {'id': id_zanra, 'ime': ime_zanra}
                dolocitve_zanra.add((id_zanra, id_filma))

    vloge = [{'igralec': id_igralca, 'film': id_filma}
             for id_igralca, id_filma in vloge]
    dolocitve_zanra = [{'zanr': id_zanra, 'film': id_filma}
             for id_zanra, id_filma in dolocitve_zanra]

    orodja.zapisi_tabelo(filmi.values(), ['id', 'naslov', 'leto', 'ocena'],
                         'csv-datoteke/filmi.csv')
    orodja.zapisi_tabelo(igralci.values(), ['id', 'ime'], 'csv-datoteke/igralci.csv')
    orodja.zapisi_tabelo(zanri.values(), ['id', 'ime'], 'csv-datoteke/zanri.csv')
    orodja.zapisi_tabelo(vloge, ['igralec', 'film'], 'csv-datoteke/vloge.csv')
    orodja.zapisi_tabelo(dolocitve_zanra, ['film', 'zanr'], 'csv-datoteke/dolocitve_zanra.csv')


def uredi_film(film):
    podatki = film.groupdict()

    regex_igralca = re.compile(
        r'<a href="/name/nm(?P<id>\w+)/">(?P<ime>.+?)</a>'
    )
    podatki['igralci'] = {
        igralec.group('id'): igralec.group('ime')
        for
        igralec in re.finditer(regex_igralca, podatki['igralci'])
    }

    regex_zanra = re.compile(
        r'<a href="/genre/(?P<id>\w+)">(?P<ime>.+?)</a>'
    )
    podatki['zanri'] = {
        zanr.group('id'): zanr.group('ime')
        for
        zanr in re.finditer(regex_zanra, podatki['zanri'])
    }

    return podatki['id'], podatki


def pripravi_rotten():
    regex_filma = re.compile(
        # r'<tr> <td class="bold">(?P<rang>\d+).</td>.*?'
        r'<tr> <td class="bold">\d+.</td>.*?'
        r'<span class="tMeterScore">(?P<ocena>\d+)%</span>.*?'
        r'<a.*?href="/m/(?P<id>.*?)/">'
        r'(?P<naslov>.*?) \((?P<leto>\d{4})\)</a>'
    )

    filmi = {}

    for html_datoteka in orodja.datoteke('zajete-strani/rotten/'):
        for film in re.finditer(regex_filma, orodja.vsebina_datoteke(html_datoteka)):
            podatki = film.groupdict()
            filmi[podatki['id']] = podatki

    orodja.zapisi_tabelo(filmi.values(), ['id', 'naslov', 'leto', 'ocena'],
                         'csv-datoteke/rotten.csv')


zajemi_imdb()
zajemi_rotten()
pripravi_imdb()
pripravi_rotten()
