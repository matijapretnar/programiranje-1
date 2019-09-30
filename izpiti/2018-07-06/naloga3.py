from functools import lru_cache


def simetricen(w):
    return w == w[::-1]


def vsotno_simetricen(w):
    if len(w) <= 1:
        return True
    l = [int(c) for c in w]
    n = int(len(l) / 2)
    return sum(l[:n]) == sum(l[n:])


@lru_cache(maxsize=None)
def stevilo_delov(w, je_simetricen):
    if w == []:
        return 0
    if je_simetricen(w):
        return 1

    options = [stevilo_delov(w[:i], je_simetricen) +
               stevilo_delov(w[i:], je_simetricen) for i in range(1, len(w))]

    return min(options)


@lru_cache(maxsize=None)
def razdeli(w, f):
    if len(w) == 0:
        return (0, [w])
    if f(w):
        return (1, [w])

    options = None
    for i in range(1, len(w)):
        nl, wl = razdeli(w[:i], f)
        nr, wr = razdeli(w[i:], f)
        k, ws = nl + nr, wl + wr

        if options is None:
            options = (k, ws)

        if k < options[0]:
            options = (k, ws)

    return options
