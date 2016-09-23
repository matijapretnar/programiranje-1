def narisi(vrstice, stolpci, macke, siri, pot):
    zemljevid = [["   " for _ in range(stolpci)] for _ in range(vrstice)]
    for (x, y) in pot:
        zemljevid[x][y] = " 👣 "
    zemljevid[0][0] = " 🐭 "
    zemljevid[-1][-1] = " 🏡 "
    for (x, y) in macke:
        zemljevid[x][y] = " 🐈 "
    for (x, y) in siri:
        zemljevid[x][y] = " 🍽 " if zemljevid[x][y] == " 👣 " else " 🧀 "
    print("\n".join("".join(vrsta) for vrsta in zemljevid))


def najvec_sira(vrstice, stolpci, macke, siri):
    pozrto = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    kam_moram = [[' ' for _ in range(stolpci)] for _ in range(vrstice)]
    for st in range(stolpci - 1, -1, -1):
        for vr in range(vrstice - 1, -1, -1):
            if (vr, st) not in macke:
                # nisem ne v zadnji vrstici ne v zadnjem stolpcu in sta dve moznosti
                if vr < vrstice - 1 and st < stolpci - 1 and \
                   pozrto[vr + 1][st] is not None and \
                   pozrto[vr][st + 1] is not None:
                    if pozrto[vr + 1][st] > pozrto[vr][st + 1]:
                        pozrto[vr][st] = pozrto[vr + 1][st]
                        kam_moram[vr][st] = 'v'
                    else:
                        pozrto[vr][st] = pozrto[vr][st + 1]
                        kam_moram[vr][st] = '>'
                # nisem v zadnji vrstici in sem v zadnjem stolpcu
                elif (st < stolpci - 1 and pozrto[vr][st + 1] is None) or vr < vrstice - 1:
                    pozrto[vr][st] = pozrto[vr + 1][st]
                    kam_moram[vr][st] = 'v'
                # sem v zadnji vrstici in nisem v zadnjem stolpcu
                elif (vr < vrstice - 1 and pozrto[vr + 1][st] is None) or st < stolpci - 1:
                    pozrto[vr][st] = pozrto[vr][st + 1]
                    kam_moram[vr][st] = '>'
                # pogledam, če sem na sirčku
                if (vr, st) in siri:
                    if pozrto[vr][st] is not None:
                        pozrto[vr][st] += 1
            else:
                pozrto[vr][st] = None
                kam_moram[vr][st] = 'x'

    vr = st = 0
    pot = [(0, 0)]
    while (vr, st) != (vrstice - 1, stolpci - 1):
        if kam_moram[vr][st] == 'v':
            vr += 1
        elif kam_moram[vr][st] == '>':
            st += 1
        pot.append((vr, st))

    # narisi(vrstice, stolpci, macke, siri, pot)

    # for vrstica in kam_moram:
    #     print("".join(vrstica))
    return pozrto[0][0]

# print(najvec_sira(5, 6, [(2, 4), (3, 3)], [(1, 4), (2, 2), (3, 2), (4, 4)]))
# print(najvec_sira(5, 6, [(2, 4), (3, 3)], [(2, 3)]))

def memo(f):
    ze_izracunani = {}
    def memo_f(*x):
        if x in ze_izracunani:
            return ze_izracunani[x]
        else:
            rez = f(*x)
            ze_izracunani[x] = rez
            return rez

    return memo_f


@memo
def najvec_sira_rek(vrstice, stolpci, macke, siri):
    if (stolpci - 1, vrstice - 1) not in macke:
        # nisem ne v zadnji vrstici ne v zadnjem stolpcu in sta dve moznosti

        if vrstice > 1 and stolpci > 1 and \
           najvec_sira_rek(vrstice - 1, stolpci, macke, siri) is not None and \
           najvec_sira_rek(vrstice, stolpci - 1, macke, siri) is not None:
           pozrto = max(najvec_sira_rek(vrstice - 1, stolpci, macke, siri),
                        najvec_sira_rek(vrstice, stolpci - 1, macke, siri))



        # nisem v zadnji vrstici in sem v zadnjem stolpcu
        elif (stolpci > 1 and najvec_sira_rek(vrstice, stolpci - 1, macke, siri) is None) or vrstice > 1:
            pozrto = najvec_sira_rek(vrstice - 1, stolpci, macke, siri)
        elif (vrstice > 1 and najvec_sira_rek(vrstice - 1, stolpci, macke, siri) is None) or stolpci > 1:
            pozrto = najvec_sira_rek(vrstice, stolpci - 1, macke, siri)
        else:
            pozrto = 0

        if (stolpci - 1, vrstice - 1) in siri:
            if pozrto is not None:
                pozrto += 1
        return pozrto
    else:
        return None

# print(najvec_sira_rek(5, 6, [(2, 4), (3, 3)], [(1, 4), (2, 2), (3, 2), (4, 4)]))
# print(najvec_sira_rek(5, 6, [(2, 4), (3, 3)], [(2, 3)]))

print(najvec_sira(8, 8, ((2, 4), (3, 3)), ((1, 4), (2, 2), (3, 2), (4, 4))))
print(najvec_sira_rek(8, 8, ((2, 4), (3, 3)), ((1, 4), (2, 2), (3, 2), (4, 4))))
# print(najvec_sira(8, 8, [(2, 4), (3, 3)], [(1, 4), (2, 2), (3, 2), (4, 4)]))
# print(najvec_sira_rek(8, 8, [(2, 4), (3, 3)], [(1, 4), (2, 2), (3, 2), (4, 4)]))
