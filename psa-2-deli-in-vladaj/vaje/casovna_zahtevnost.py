"""
Eksperimentalna vaja: Časovna zahtevnost algoritmov

Izmerili bomo čas, ki ga za sortiranje seznama potrebujejo algoritmi,
implementirani v `urejanje.py`. Rezultate bomo narisali na skupnem grafu.
"""

import time  # Štoparica
import matplotlib.pyplot as plt  # Risanje grafov
plt.style.use('ggplot') # Lepi grafi
import urejanje  # Implementacije algoritmov

# Uporabili bomo lasten generator naključnih števil, da bodo le-ta neodvisna
# od implementacije intepreterja za Python.

def nakljucna_stevila(seme):
    """
    Linearni kongruenčni generator psevdonaključnih števil.
    """
    m = 2**64  # Modul
    a = 6364136223846793005 % m  # Množitelj
    c = 1442695040888963407 % m  # Inkrement
    x = seme % m
    while True:
        x = (a*x + c) % m
        yield x

# Sestavimo seznam 10^6 "naključnih števil" med 0 in 10^6 - 1.
g = nakljucna_stevila(31337)
stevila = [x % 10**6 for _, x in zip(range(10**6), g)]

def je_urejen(l):
    """
    Preveri, če je seznam l urejen.
    """
    n = len(l)
    for i in range(0, n-1):
        if l[i] > l[i+1]:
            return False
    return True

def izmeri(algo, ime_algoritma, dolzine):
    """
    Vrni seznam časov, ki jih algoritem potrebuje za urejanje seznamov dolzin dolzine.
    """
    casi = []
    for i in dolzine:
        # Pripravimo si seznam števil
        seznam = stevila[:i]
        prej = time.time()
        algo(seznam)
        if not je_urejen(seznam):
            raise ValueError("algoritem '{}' ne deluje pravilno".format(ime_algoritma))
        potem = time.time()
        casi.append(potem - prej)
    return casi

dolzine = list(range(10**1, 10**2+1, 10**1))

t1 = izmeri(urejanje.naivno_uredi, 'Naivni sort', dolzine)
t2 = izmeri(urejanje.vgrajeni_sort, 'Vgrajeni sort', dolzine)

# Nariši graf z rezultati
plt.plot(dolzine, t1, label="Naivni")
plt.plot(dolzine, t2, label="Vgrajeni")
plt.legend()
plt.show()
