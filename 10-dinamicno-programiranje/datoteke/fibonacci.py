# NAIVNA IMPLEMENTACIJA Z REKURZIJO

# Že izračun fib_z_rekurzijo(35) traja zelo veliko časa.


def fib_z_rekurzijo(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib_z_rekurzijo(n - 1) + fib_z_rekurzijo(n - 2)


# MEMOIZACIJA

# Ideja memoizacije: imejmo slovar, v katerega si bomo shranili že izračunane rezultate.

ze_izracunani = {}


def fib_s_slovarjem(n):
    if n in ze_izracunani:
        return ze_izracunani[n]
    else:
        if n == 0:
            rez = 0
        elif n == 1:
            rez = 1
        else:
            rez = fib_s_slovarjem(n - 1) + fib_s_slovarjem(n - 2)
        ze_izracunani[n] = rez
        return rez

# Z memoizacijo lahko hitro izračunamo fib_memo pri vrednostih do 300
# (odvisno od različice Pythona), nato pa nam Python sporoči, da je naša
# rekurzija šla pregloboko.

# Zgornjo funkcijo lahko pišemo tudi kot:


def fib_s_slovarjem2(n):
    def fib(n):
        if n == 0:
            return 0
        elif n == 1:
            return 1
        else:
            return fib_s_slovarjem2(n - 1) + fib_s_slovarjem2(n - 2)

    if n not in ze_izracunani:
        ze_izracunani[n] = fib(n)
    return ze_izracunani[n]


# MEMOIZACIJA S FUNKCIJO VIŠJEGA REDA

# Ta postopek lahko naredimo za splošno funkcijo f in sicer tako, da si napišemo
# funkcijo višjega reda memo, ki vzame f in vrne funkcijo memo_f, ki računa
# enako kot f, le da si zapomni svoje rezultate.

def memo(f):
    ze_izracunani = {}
    def memo_f(*x):  # če pišemo *x, se bo v x shranil nabor vseh argumentov,
                     # s čimer omogočimo, da memo deluje tudi za funkcije več
                     # kot enega argumenta.
        if x not in ze_izracunani:
            ze_izracunani[x] = f(*x)
        return ze_izracunani[x]

    return memo_f

def glasna(n):
    print("Računam", n)
    return n

# Če pokličemo spodnje ukaze, bomo videli, da funkcija trikrat računa rezultat.
glasna(1)
glasna(1)
glasna(1)

# Zdaj vidimo, da se rezultat izračuna le enkrat.
memo_glasna = memo(glasna)
memo_glasna(1)
memo_glasna(1)
memo_glasna(1)

# Toda tole ni nič bolj učinkovito:
memo_fib = memo(fib_z_rekurzijo)
# print(memo_fib(30))

# Zakaj? Ker fib_z_rekurzijo pokliče samega sebe, ne pa memo_fib. Če pogledate
# definicijo fib_s_slovarjem2, vidite, da notranja funkcija fib spet kliče
# fib_s_slovarjem2, zato ima dostop do že izračunanih vrednosti. Funkcijo
# fib_z_rekurzijo bi lahko popravili, da poleg argumenta n sprejme še funkcijo,
# ki naj jo kliče rekurzivno, vendar je v Pythonu enostavneje uporabiti
# dekorator, ki definicijo funkcije popravi tako, da se vsi njeni klici nanašajo
# na spremenjeno funkcijo.

@memo
def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

# Potem se npr. fib(299) izračuna hitro.
print(fib(299))
