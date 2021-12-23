mat = [[131, 673, 234, 103, 18],
[201, 96, 342, 965, 150],
[630, 803, 746, 422, 111],
[537, 699, 497, 121, 956],
[805, 732, 524, 37, 331]]


def najdrazja_pot(mat):
    m, n = len(mat), len(mat[0])
    cene_poti = [[None for _ in vrstica] for vrstica in mat]
    cene_poti[-1][-1] = mat[-1][-1]
    for j in range(n - 2, -1, -1):
        cene_poti[-1][j] = cene_poti[-1][j + 1] + mat[-1][j]
    for i in range(m - 2, -1, -1):
        cene_poti[i][-1] = cene_poti[i + 1][-1] + mat[i][-1]
        for j in range(n - 2, -1, -1):
            cene_poti[i][j] = max(
                cene_poti[i + 1][j],
                cene_poti[i][j + 1]
            ) + mat[i][j]
    return cene_poti


def najdrazja_pot(mat):
    m, n = len(mat), len(mat[0])
    cene_poti = [[None for _ in vrstica] for vrstica in mat]
    cene_poti[-1][-1] = (mat[-1][-1], "/")
    for j in range(n - 2, -1, -1):
        cene_poti[-1][j] = (cene_poti[-1][j + 1][0] + mat[-1][j], "→")
    for i in range(m - 2, -1, -1):
        cene_poti[i][-1] = (cene_poti[i + 1][-1][0] + mat[i][-1], "↓")
        for j in range(n - 2, -1, -1):
            cene_poti[i][j] = max(
                (cene_poti[i + 1][j][0] + mat[i][j], "↓"),
                (cene_poti[i][j + 1][0] + mat[i][j], "→")
            ) 
    return cene_poti

def dodaj_korak(cena, korak, pot):
    cena_poti, koraki_poti = pot
    return cena + cena_poti, korak + koraki_poti

def najdrazja_pot(mat):
    m, n = len(mat), len(mat[0])
    poti = [[None for _ in vrstica] for vrstica in mat]
    poti[-1][-1] = (mat[-1][-1], "o")
    for j in range(n - 2, -1, -1):
        poti[-1][j] = dodaj_korak(mat[-1][j], "→", poti[-1][j + 1])
    for i in range(m - 2, -1, -1):
        poti[i][-1] = dodaj_korak(mat[i][-1], "↓", poti[i + 1][-1])
        for j in range(n - 2, -1, -1):
            poti[i][j] = min(
                dodaj_korak(mat[i][j], "↓", poti[i + 1][j]),
                dodaj_korak(mat[i][j], "→", poti[i][j + 1])
            ) 
    return poti[0][0]

print(najdrazja_pot(mat))

