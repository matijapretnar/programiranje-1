from functools import cache

def najcenejsa_pot(mat):
    m, n = len(mat), len(mat[0])
    @cache
    def pomozna(i, j):
        if i == m - 1 and j == n - 1:
            return (mat[-1][-1], "o")
        else:
            moznosti = []
            if i < m - 1:
                cena_dol, pot_dol = pomozna(i + 1, j) 
                moznosti.append((cena_dol, "↓" + pot_dol))
            if j < n - 1:
                cena_desno, pot_desno = pomozna(i, j + 1) 
                moznosti.append((cena_desno, "→" + pot_desno))
            cena, pot = min(moznosti)
            return mat[i][j] + cena, pot
    return pomozna(0, 0)

mat = [[131, 673, 234, 103, 18],
[201, 96, 342, 965, 150],
[630, 803, 746, 422, 111],
[537, 699, 497, 121, 956],
[805, 732, 524, 37, 331]]