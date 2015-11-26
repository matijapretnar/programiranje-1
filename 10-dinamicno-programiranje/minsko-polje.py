def vse_poti_imp(vrstice, stolpci, mine):
    poti = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    poti[0][0] = 1
    for stolpec in range(0, stolpci):
        for vrstica in range(0, vrstice):
            if (vrstica, stolpec) in mine:
                continue
            if vrstica > 0:
                poti[vrstica][stolpec] += poti[vrstica - 1][stolpec]
            if stolpec > 0:
                poti[vrstica][stolpec] += poti[vrstica][stolpec - 1]
    return poti[vrstice - 1][stolpci - 1]
