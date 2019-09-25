"""
Eksperimentalna vaja: Časovna zahtevnost metod.
"""

import time                         # Štoparica
import matplotlib.pyplot as plt     # Risanje grafov


# Funkcija, ki meri čas, potreben za izvajanje.
def measure(algo, data):
    """
    Measures the time needed for the execution of [algo] on [data].
    """
    before = time.time()
    algo(data)
    after = time.time()
    return (after - before)

# Seznami različnih velikosti, ki jih lahko podamo kot vhod testnim funkcijam.
small = list(range(10**2, 10**3+1, 2*10))
medium = list(range(10**3, 10**4+1, 2*10**2))
large = list(range(10**4, 10**5+1, 2*10**3))
xlarge = list(range(10**5, 10**6+1, 2*10**4))

###############################################################################
# Funkcije, ki sestavijo seznam oz. slovar, ki vsebuje elemente od 1 do n.
# Funkcija [add_back] dodaja elemente na konec seznama, [add_front] pa na
# začetek. V slovar vstavljamo elemente kot ponavadi.
###############################################################################


def add_back(n):
    l = []
    for i in range(n):
        l.append(n)
    return


def add_front(n):
    l = []
    for i in range(n):
        l = [n] + l
    return


def add_dict(n):
    d = dict()
    for i in range(n):
        d[i] = i
    return

###############################################################################
# 1.) [test_add_back(test_sizes)] nariše graf potrebnega časa, da zgradimo
# seznam določene dolžine z dodajanjem na konec.
# Če je časovna zahtevnost izgranje seznama O(n) premisli kolikšna je časovna
# zahtevnost enega samega vstavljanja.
#
# Priporočen vhod: large in xlarge
#
###############################################################################


def test_add_back(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    plt.plot(test_sizes, times1, 'r')
    plt.show()

###############################################################################
# 2.) [test_add_compare(test_sizes)] primerja časovni zahtevnosti dodajanja na
# konec in dodajanja na začetek.
# Pri tem se spomni, da iz prejšnjega testa veš, da dodajanje na konec ni
# konstantno, kot se morda zdi pri tem testu.
# Spet premisli kolikšna je časovna zahtevnost enega dodajanja na začetek.
#
# Priporočen vhod: medium
#
###############################################################################


def test_add_compare(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    times2 = []
    for size in test_sizes:
        times2.append(measure(add_front, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()


###############################################################################
# 3.) [test_add_list_vs_dict(test_sizes)] primerja učinkovitost izgradnje
# seznama in slovarja.
#
# Slovarji uporabljajo tako imenovano 'hash' funkcijo, s katero določajo
# kam vstaviti nov element. Če je hash funkcija dobra (torej ne slika veliko
# elementov v isto polje) se to zgodi hitro. Ko pa pride do preveč križanj,
# (zaradi veliko novih elementov) zamenja hash funkcijo in poveča tabelo v
# kateri hrani elemente in s tem zmanjša število križanj.
#
# Priporočen vhod: large
#
###############################################################################


def test_add_list_vs_dict(test_sizes):
    times1 = []
    for size in test_sizes:
        times1.append(measure(add_back, size))

    times2 = []
    for size in test_sizes:
        times2.append(measure(add_dict, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()


###############################################################################
# 4.) [test_find_list_vs_dict(test_sizes)] primerja učinkovitost iskanja nekega
# elementa v seznamu ali slovarju. V testu imamo shranjena števila od 1 do n in
# vedno iščemo število n, kar je najslabši primer iskanja za seznam.
# Izvedemo 1000 iskanj (lahko spremeniš kot pomožni parameter).
#
# Ker slovar s hash funkcijo določa kam shrani element, lahko z isto hash
# funkcijo pogleda, kje bi element moral biti.
#
# Priporočen vhod: medium
#
# 5.) Prilagodi funkcijo tako, da na graf nariše zgolj vrednosti za slovar.
# Kakšna se ti zdi časovna zahtevnost iskanja v slovarju?
#
# 6.) Sedaj prilagodi funkcijo tako, da v seznamu vedno iščeš število 0.
# Kako se spremeni časovna zahtevnost iskanja? Zakaj je to najhitrejši primer
# za slovar?
#
# 7.) Kaj se zgodi če namesto iskanja števila n v slovarju iščemo število na
# indeksu n. Ali se to ujema s tipom List v OCamlu?
#
###############################################################################


def test_find_list_vs_dict(test_sizes, times=1000):
    times1 = []
    times2 = []
    # Ne želimo da se seznam oz. slovar po katerem iščemo ustvari v algoritmu,
    # zato ga definiramo pred definicijo algoritma iskanja.
    temp_l = []
    temp_d = dict()

    def find_dict(n):
        for i in range(times):
            # Poišče element (n-1)
            temp_d.get(n-1, 0)
        return

    def find_list(n):
        for i in range(times):
            # Poišče indeks za vrednost (n-1) kar je ekvivalentno temu, da
            # preveri če je (n-1) element seznama.
            temp_l.index(n-1)
        return

    for size in test_sizes:
        # Ustvari seznam in slovar za iskanje preden začnemo meriti čas.
        temp_l = [x for x in range(size)]
        temp_d = dict((x, x) for x in range(size))
        times1.append(measure(find_list, size))
        times2.append(measure(find_dict, size))

    plt.plot(test_sizes, times1, 'r', test_sizes, times2, 'b')
    plt.show()
