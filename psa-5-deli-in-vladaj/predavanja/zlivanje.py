def uredi(sez):
    n = len(sez)
    if n <= 1:
        return sez
    else:
        return zlij(uredi(sez[:n // 2]), uredi(sez[n // 2:]))


def zlij(sez1, sez2):
    n1 = len(sez1)
    n2 = len(sez2)
    zliti_sez = (n1 + n2) * [None]
    i = i1 = i2 = 0
    while i1 < n1 or i2 < n2:
        if i1 == n1:
            zliti_sez[i] = sez2[i2]
            i2 += 1
        elif i2 == n2:
            zliti_sez[i] = sez1[i1]
            i1 += 1
        elif sez1[i1] <= sez2[i2]:
            zliti_sez[i] = sez1[i1]
            i1 += 1
        else:
            assert sez1[i1] > sez2[i2]
            zliti_sez[i] = sez2[i2]
            i2 += 1
        i += 1
    return zliti_sez


def uredi_na_mestu(sez, zac=None, kon=None):
    n = len(sez)
    if zac is None:
        zac = 0
    if kon is None:
        kon = n
    if kon - zac <= 1:
        return
    else:
        sre = (zac + kon) // 2
        uredi_na_mestu(sez, zac, sre)
        uredi_na_mestu(sez, sre, kon)
        zlij_na_mestu(sez, zac, sre, kon)


def zlij_na_mestu(sez, zac, sre, kon):
    zliti_sez = (kon - zac) * [None]
    i1, i2, i = zac, sre, 0
    while i1 < sre or i2 < kon:
        if i1 == sre:
            zliti_sez[i] = sez[i2]
            i2 += 1
        elif i2 == kon:
            zliti_sez[i] = sez[i1]
            i1 += 1
        elif sez[i1] <= sez[i2]:
            zliti_sez[i] = sez[i1]
            i1 += 1
        else:
            assert sez[i1] > sez[i2]
            zliti_sez[i] = sez[i2]
            i2 += 1
        i += 1
    sez[zac:kon] = zliti_sez


import timeit

print(timeit.timeit('uredi(list(range(1000, 0, -1)))',
      'from __main__ import uredi', number=50))
print(timeit.timeit('uredi(list(range(2000, 0, -1)))',
      'from __main__ import uredi', number=50))
print(timeit.timeit('uredi(list(range(4000, 0, -1)))',
      'from __main__ import uredi', number=50))
print(timeit.timeit('uredi(list(range(8000, 0, -1)))',
      'from __main__ import uredi', number=50))
print(timeit.timeit('uredi_na_mestu(list(range(1000, 0, -1)))',
      'from __main__ import uredi_na_mestu', number=50))
print(timeit.timeit('uredi_na_mestu(list(range(2000, 0, -1)))',
      'from __main__ import uredi_na_mestu', number=50))
print(timeit.timeit('uredi_na_mestu(list(range(4000, 0, -1)))',
      'from __main__ import uredi_na_mestu', number=50))
print(timeit.timeit('uredi_na_mestu(list(range(8000, 0, -1)))',
      'from __main__ import uredi_na_mestu', number=50))
