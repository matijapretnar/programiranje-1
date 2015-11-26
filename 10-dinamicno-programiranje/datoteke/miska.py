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
