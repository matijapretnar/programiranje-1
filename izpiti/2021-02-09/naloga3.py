from functools import lru_cache

# =============================================================================
# Psička Nara po njivi preganja krokarje. Opazila je, da jo lastnik čaka na
# drugem koncu polja, zato hiti k njemu, pri tem pa hoče prestrašiti kar se da
# veliko ubogih ptičev.
#
# Njivo predstavimo kot matriko, ki v vsakem polju vsebuje število krokarjev,
# ki jih pasja navihanka prežene, če teče preko tega polja.
# =============================================================================

primer = [
    [2, 3, 0, 2, 9],
    [8, 3, 5, 1, 2],
    [1, 2, 7, 2, 0],
    [4, 3, 6, 5, 5],
]

# (a)
# =============================================================================
# Nara se nahaja v zgornjem levem kotu njive (polje `(0, 0)`). Ker se ji mudi
# k lastniku, se vztrajno premika desno. Na vsakem koraku se lahko premakne:
#   - desno
#   - diagonalno desno-gor
#   - diagonalno desno-dol
#
# Pregon krokarjev zaključi na poljubnem skrajno desnem polju njive. Napišite
# funkcijo, ki izračuna največje število krokarjev, ki jih lahko nagajivka
# prežene.
# =============================================================================

def nara1(polje):
    @lru_cache(maxsize=None)
    def nara(i, j):
        if i < len(polje[0]) and 0 <= j < len(polje):
            naj = max([nara(i+1, j+1), nara(i+1, j), nara(i+1, j-1)])
            return naj + polje[j][i]
        else:
            return 0
    return nara(0, 0)


# (b)
# =============================================================================
# Funkcijo iz točke (a) prilagodite tako, da ji dodatno podate indeks vrstice,
# v kateri Nara začne, in indeks vrstice, v kateri Nara konča.
#
# Funkcija naj vrne seznam VSEH optimalnih poti, kjer pot predstavimo s
# seznamom indeksov polj, preko katerih Nara teče.
# =============================================================================
def nara2(polje, j_start, j_end):
    @lru_cache(maxsize=None)
    def nara(i, j):
        if i == len(polje[0]) - 1 and j == j_end:
            return polje[j][i], [[(i, j)]]
        elif i < len(polje[0]) and 0 <= j < len(polje):
            moznosti = [nara(i+1, j+1), nara(i+1, j), nara(i+1, j-1)]
            naj, _ = max(moznosti)
            poti = [[(i, j)]+pot for v, p in moznosti if v == naj for pot in p]
            return naj + polje[j][i], poti
        else:
            return float("-inf"), []
    return nara(0, j_start)
