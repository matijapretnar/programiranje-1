import orodja
import re


for stran in range(1, 50):
    osnovni_naslov = 'http://www.imdb.com/search/title'
    parametri = 'sort=user_rating,desc&title_type=feature&num_votes=50000,'
    naslov = '{}?{}&page={}'.format(osnovni_naslov, parametri, stran)
    datoteka = 'imdb/{:02}.html'.format(stran)
    orodja.shrani(naslov, datoteka)
    with open(datoteka) as f:
        vsebina = f.read()
        for ujemanje in re.finditer('\(\d{4}\)', vsebina):
            print(ujemanje)
