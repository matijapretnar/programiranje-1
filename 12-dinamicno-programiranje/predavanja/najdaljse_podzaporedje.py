def najdaljse_skupno_podzaporedje(xs, ys, i=0, j=0):
    if i >= len(xs) or j >= len(ys):
        return []
    elif xs[i] == ys[j]:
        return [xs[i]] + najdaljse_skupno_podzaporedje(xs, ys, i + 1, j + 1)
    else:
        brez_x = najdaljse_skupno_podzaporedje(xs, ys, i + 1, j)
        brez_y = najdaljse_skupno_podzaporedje(xs, ys, i, j + 1)
        return brez_x if len(brez_x) >= len(brez_y) else brez_y

print(najdaljse_skupno_podzaporedje([1, 3, 2, 5, 10, 20, 30, 5, 4, 2, 7], [1, 2, 3, 5, 10, 30, 20, 30, 1, 9, 3, 13]))