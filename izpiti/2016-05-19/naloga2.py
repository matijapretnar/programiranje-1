#!/usr/bin/python3

# 2. naloga (Zlaganje kock)


# a)
# Časovna zahtevnost: O(n)
def st_stolpov(n):
    if n == 0:
        return 1
    ena = [0 for i in range(max(n + 1, 4))]
    dva = [0 for i in range(max(n + 1, 4))]
    tri = [0 for i in range(max(n + 1, 4))]
    ena[1] = ena[3] = dva[2] = dva[3] = tri[3] = 1
    for i in range(4, n + 1):
        ena[i] = dva[i - 1] + tri[i - 1]
        dva[i] = ena[i - 2] + tri[i - 2]
        tri[i] = ena[i - 3] + dva[i - 3]
    return ena[n] + dva[n] + tri[n]


# b)
# Časovna zahtevnost: O(n)
def barvni_stolpi(n):
    if n == 0:
        return 1
    modra = [0 for i in range(max(n + 1, 4))]
    rdeca = [0 for i in range(max(n + 1, 4))]
    rdeca[1:4] = [1, 1, 3]
    modra[1:4] = [1, 2, 2]
    for i in range(4, n + 1):
        rdeca[i] = modra[i - 1] + modra[i - 3]
        modra[i] = rdeca[i - 1] + rdeca[i - 2]
    return modra[n] + rdeca[n]
