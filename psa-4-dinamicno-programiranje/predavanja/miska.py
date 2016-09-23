def stevilo_poti(vrstice, stolpci, macke):
    # poti[x][y] je stevilo poti od (x, y) do cilja
    poti = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    poti[vrstice - 1][stolpci - 1] = 1
    for stolpec in range(stolpci - 1, -1, -1):
        for vrstica in range(vrstice - 1, -1, -1):
            if (vrstica, stolpec) in macke:
                continue
            if vrstica < vrstice - 1:
                poti[vrstica][stolpec] += poti[vrstica + 1][stolpec]
            if stolpec < stolpci - 1:
                poti[vrstica][stolpec] += poti[vrstica][stolpec + 1]

    return poti[0][0]

print(stevilo_poti(5, 6, {(1, 4), (2, 2), (3, 3)}))

print(stevilo_poti(3, 3, {(1, 0)}))

print(stevilo_poti(20, 12, set()))  # moralo bi biti enako 32! / (20! * 12!)


def stevilo_poti_alt(vrstice, stolpci, macke):
    '''Alternativna rešitev'''
    # poti[x][y] je število poti od začetka do (x, y)
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

# moralo bi vrniti isto kot zgoraj
print(stevilo_poti_alt(5, 6, {(1, 4), (2, 2), (3, 3)}))

print(stevilo_poti_alt(3, 3, {(1, 0)}))

print(stevilo_poti_alt(20, 12, set()))  # moralo bi biti enako 32! / (20! * 12!)


def najvec_sira(vrstice, stolpci, siri):
    pozrto = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    pozrto[0][0] = 0
    for stolpec in range(0, stolpci):
        for vrstica in range(0, vrstice):
            if vrstica > 0:
                pozrto[vrstica][stolpec] = max(pozrto[vrstica][stolpec],
                                               pozrto[vrstica - 1][stolpec])
            if stolpec > 0:
                pozrto[vrstica][stolpec] = max(pozrto[vrstica][stolpec],
                                               pozrto[vrstica][stolpec - 1])
            if (vrstica, stolpec) in siri:
                pozrto[vrstica][stolpec] += 1
    return pozrto[vrstice - 1][stolpci - 1]

print(najvec_sira(10, 10, {(5, 5), (4, 5), (5, 4), (6, 6)}))


def kako_do_najvec_sira(vrstice, stolpci, siri):
    pozrto = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    kako = [[None for _ in range(stolpci)] for _ in range(vrstice)]
    pozrto[0][0] = 0
    kako[0][0] = "O"
    for stolpec in range(0, stolpci):
        for vrstica in range(0, vrstice):
            if vrstica > 0:
                if pozrto[vrstica][stolpec] <= pozrto[vrstica - 1][stolpec]:
                    pozrto[vrstica][stolpec] = pozrto[vrstica - 1][stolpec]
                    kako[vrstica][stolpec] = 'v'
            if stolpec > 0:
                if pozrto[vrstica][stolpec] <= pozrto[vrstica][stolpec - 1]:
                    pozrto[vrstica][stolpec] = pozrto[vrstica][stolpec - 1]
                    kako[vrstica][stolpec] = '>'
            if (vrstica, stolpec) in siri:
                pozrto[vrstica][stolpec] += 1
    print("\n".join("".join(vrstica) for vrstica in kako))
    return pozrto[vrstice - 1][stolpci - 1]

print(kako_do_najvec_sira(10, 10, {(5, 5), (4, 5), (5, 4), (6, 6)}))
