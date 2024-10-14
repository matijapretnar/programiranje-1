# Ta je malo podobna : https://github.com/matijapretnar/programiranje-1/blob/master/izpiti/2021-08-24/izpit-2021-08-24.pdf ampak ni kritično

"""
Kurent mora po dolgi in naporni pustni soboti (zaradi kulturne izmenjave) obiskati še Ameriško mesto, ki ga predstavimo z matriko `N x M`.
Na vsakem mestu v matriki je celo (lahko negativno) število, ki predstavlja količino `bobov`.
Ker je Kurent, seveda ne sme iti kar tako, ampak mora upoštevati nekaj pravil:
- Pot po mreži se začne v zgornjem levem kotu in konča v spodnjem desnem kotu.
- Pot začne z 0 bobi in med potjo nikoli ne sme imeti negativnega števila bobov (četudi bi ob pristanku na lokaciji število bobov spet postalo nenegativno to ni sprejemljivo).
- Po mreži se lahko premika le en korak v desno, en krak navzdol ali en korak v desno in en korak navzdol (diagonalno) in za vsak tak korak plača 1 bob.
- Enkrat v sprehodu lahko skoči na poljubno mesto (tudi nazaj na mesto, kjer stoji) v mreži, vendar za tak skok plača 3 bobe.
- Ko se kurent nahaja na nekem mestu v mreži pobere vse bobe, ki so na tem mestu (če je število bobov negativno se to odšteje od skupnega števila bobov).
- Če se kurent večkrat znajde na istem mestu, bo vsakič pobral vse bobe, ki so na tem mestu (medtem, ko je bil odsoten se bobi obnovijo).

Pomagajte kurentu naiskati pot, ob kateri konča z največjim številom bobov.
Napišite funkcijo, ki vrne največje število bobov, ki jih lahko Kurent pobere in seznam ukazov, ki jih mora izvesti, da jih pobere, pri tem "D" pomeni korak navzdol, "R" korak desno in "DI" korak diagonalno, JUMP i j pa skok na mesto i, j.

"""
from functools import lru_cache

pustni_torek = [
    [10, 20, -30, -100, 9] * 5,
    [0, 0, 50, 50, 2] * 5,
    [10, 20, 300, -10, 0] * 5,
    [40, 30, -200, -10, 5] * 5,
]*2

JUMP_BACK = 3
RIGHT = 1
DOWN = 1
DIAGONAL = 1


def najvec_bobov(matrika):
    N = len(matrika) - 1
    M = len(matrika[0]) - 1

    @lru_cache(maxsize=None)
    def aux(i, j, bobi, jumped) -> tuple[float | int, list[str]]:
        bobi = bobi + matrika[i][j]
        if bobi < 0:
            return (float("-inf"), [])
        if i == N and j == M:
            if jumped:
                return bobi, []

            options = [(bobi, [])]
            best, dirs = bobi, []
            for i in range(N + 1):
                for j in range(M + 1):
                    if bobi - JUMP_BACK > 0:
                        amount, dirs = aux(i, j, bobi - JUMP_BACK, True)
                        if amount > best:
                            best, dirs = amount, ([f"JUMP {i} {j}"] + dirs)

            return best, dirs

        options = []
        if i < N:
            options.append((i + 1, j, DOWN, "D"))
        if j < M:
            options.append((i, j + 1, RIGHT, "R"))
        if i < N and j < M:
            options.append((i + 1, j + 1, DIAGONAL, "DI"))

        assert options, f"Nismo našli nobene možnosti"

        best, dirs = float("-inf"), []
        for i, j, cost, direction in options:
            amount, dirs2 = aux(i, j, bobi - cost, jumped)
            if bobi - cost > 0 and amount > best:
                best = amount
                dirs = [direction] + dirs2
        if not jumped:
            for i in range(N + 1):
                for j in range(M + 1):
                    if bobi - JUMP_BACK > 0:
                        amount, dirs2 = aux(i, j, bobi - JUMP_BACK, True)
                        if amount > best:
                            best = amount
                            dirs = [f"JUMP {i} {j}"] + dirs2

        return best, dirs

    b = aux(0, 0, 0, False)
    print(aux.cache_info())
    return b


print(najvec_bobov(pustni_torek))
