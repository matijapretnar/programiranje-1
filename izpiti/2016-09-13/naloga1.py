#!/usr/bin/python3

def je_mogoce(t, Z, T):
    konec = float('-inf')
    for x in t:
        zacetek = max(x, konec)
        konec = zacetek + T
        # print(round(konec, 4))
    return konec <= Z

def strizenje(t, Z):
    a = 0
    b = Z - t[-1]
    while b - a > 10**-6:
        c = (a + b) / 2
        if je_mogoce(t, Z, c):
            a = c
        else:
            b = c
    return a

print(strizenje([0.1, 2.5, 6.2, 8.0, 10.0, 13.0], 16.0))

print(je_mogoce([0.1, 2.5, 6.2, 8.0, 10.0, 13.0], 16.0, 2.3))
print(je_mogoce([0.1, 2.5, 6.2, 8.0, 10.0, 13.0], 16.0, 2.6))
