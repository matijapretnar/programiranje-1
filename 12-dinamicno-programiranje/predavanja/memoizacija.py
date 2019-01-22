def memoiziraj(f):
    rezultati = {}
    def mem_f(*args):
        if args not in rezultati:
            rezultati[args] = f(*args)
        return rezultati[args]
    return mem_f

@memoiziraj
def najdaljse_skupno_podzaporedje(xs, ys, i=0, j=0):
    if i >= len(xs) or j >= len(ys):
        return []
    elif xs[i] == ys[j]:
        return [xs[i]] + najdaljse_skupno_podzaporedje(xs, ys, i + 1, j + 1)
    else:
        brez_x = najdaljse_skupno_podzaporedje(xs, ys, i + 1, j)
        brez_y = najdaljse_skupno_podzaporedje(xs, ys, i, j + 1)
        return brez_x if len(brez_x) >= len(brez_y) else brez_y

print(najdaljse_skupno_podzaporedje((1, 3, 2, 5, 10, 20, 30, 5, 4, 2, 7), (1, 2, 3, 5, 10, 30, 20, 30, 1, 9, 3, 13)))

# 
def najdaljse_skupno_podzaporedje(xs, ys):
    @memoiziraj
    def pomozna(i, j):
        if i >= len(xs) or j >= len(ys):
            return []
        elif xs[i] == ys[j]:
            return [xs[i]] + pomozna(i + 1, j + 1)
        else:
            brez_x = pomozna(i + 1, j)
            brez_y = pomozna(i, j + 1)
            return brez_x if len(brez_x) >= len(brez_y) else brez_y
    return pomozna(0, 0)


print(najdaljse_skupno_podzaporedje(
    [4, 5, 2, 1, 9, 0, 4, 0, 1, 0, 7, 5, 9, 8, 5, 2, 1, 6, 8],
    [9, 3, 4, 3, 3, 4, 6, 3, 0, 0, 9, 2, 5, 1, 7, 7, 7, 0, 3],
))


def najcenejsa_pot(matrika):
    @memoiziraj
    def pomozna(i, j):
        # sem v zadnji vrstici
        if i == len(matrika) - 1:
            return sum(matrika[i][j:])
        # sem v zadnjem stolpcu
        elif j == len(matrika[i]) - 1:
            return sum(matrika[k][-1] for k in range(i, len(matrika)))
        # sem nekje vmes
        else:
            grem_dol = pomozna(i + 1, j)
            grem_desno = pomozna(i, j + 1)
            return matrika[i][j] + min(grem_dol, grem_desno)
    return pomozna(0, 0)

print(najcenejsa_pot([
    [131, 673, 234, 103, 18],
    [201, 96, 342, 965, 150],
    [630, 803, 746, 422, 111],
    [537, 699, 497, 121, 956],
    [805, 732, 524, 37, 331]
]))
