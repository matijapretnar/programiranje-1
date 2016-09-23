def stevilo_poti(vrstice, stolpci, macke):
    # poti[x][y] je stevilo poti od zacetka do (x, y)
    poti = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    poti[0][0] = 1
    for stolpec in range(0, stolpci):
        for vrstica in range(0, vrstice):
            if (vrstica, stolpec) in macke:
                continue
            if vrstica > 0:
                poti[vrstica][stolpec] += poti[vrstica - 1][stolpec]
            if stolpec > 0:
                poti[vrstica][stolpec] += poti[vrstica][stolpec - 1]
    return poti[vrstice - 1][stolpci - 1]


def stevilo_poti_pocasna(vrstice, stolpci, macke):
    if (vrstice - 1, stolpci - 1) in macke:
        stevilo = 0
    elif vrstice == 1 and stolpci == 1:
        stevilo = 1
    else:
        stevilo = 0
        if vrstice > 1:
            stevilo += stevilo_poti_pocasna(vrstice - 1, stolpci, macke)
        if stolpci > 1:
            stevilo += stevilo_poti_pocasna(vrstice, stolpci - 1, macke)
    return stevilo


izracunane_poti = {}
def stevilo_poti_memo(vrstice, stolpci, macke):
    if (vrstice, stolpci, macke) not in izracunane_poti:
        if (vrstice - 1, stolpci - 1) in macke:
            stevilo = 0
        elif vrstice == 1 and stolpci == 1:
            stevilo = 1
        else:
            stevilo = 0
            if vrstice > 1:
                stevilo += stevilo_poti_memo(vrstice - 1, stolpci, macke)
            if stolpci > 1:
                stevilo += stevilo_poti_memo(vrstice, stolpci - 1, macke)
        izracunane_poti[(vrstice, stolpci, macke)] = stevilo
    return izracunane_poti[(vrstice, stolpci, macke)]


def memo(f):
    izracunane = {}
    def memo_f(*args):
        if args not in izracunane:
            izracunane[args] = f(*args)
        return izracunane[args]
    return memo_f


@memo
def stevilo_poti_super(vrstice, stolpci, macke):
    if (vrstice - 1, stolpci - 1) in macke:
        stevilo = 0
    elif vrstice == 1 and stolpci == 1:
        stevilo = 1
    else:
        stevilo = 0
        if vrstice > 1:
            stevilo += stevilo_poti_super(vrstice - 1, stolpci, macke)
        if stolpci > 1:
            stevilo += stevilo_poti_super(vrstice, stolpci - 1, macke)
    return stevilo


print(stevilo_poti(20, 7, ((1, 2), (3, 4))))
print(stevilo_poti_pocasna(20, 7, ((1, 2), (3, 4))))
print(stevilo_poti_memo(20, 7, ((1, 2), (3, 4))))
print(stevilo_poti_super(20, 7, ((1, 2), (3, 4))))
