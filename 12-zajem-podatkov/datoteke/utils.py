import os
import re
import requests
import sys


def shrani(url, ime_datoteke, vsili_prenos=False):
    try:
        print('Shranjujem {}...'.format(url), end='')
        sys.stdout.flush()
        if os.path.isfile(ime_datoteke) and not vsili_prenos:
            print('shranjeno že od prej!')
            return
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        print('stran ne obstaja!')
    os.makedirs(os.path.dirname(ime_datoteke), exist_ok=True)
    with open(ime_datoteke, 'w') as datoteka:
        datoteka.write(r.text)
        print('shranjeno!')


def ujemanja(regex, ime_datoteke):
    with open(ime_datoteke) as datoteka:
        vsebina = datoteka.read()
    return re.finditer(regex, vsebina)


def datoteke(imenik):
    return [os.path.join(imenik, datoteka) for datoteka in os.listdir(imenik)]
