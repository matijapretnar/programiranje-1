def najdaljse_skupno(xs, ys):
    if not xs or not ys:
        return ()
    elif xs[0] == ys[0]:
        return (xs[0],) + najdaljse_skupno(xs[1:], ys[1:])
    else:
        l = najdaljse_skupno(xs, ys[1:])
        d = najdaljse_skupno(xs[1:], ys)
        return l if len(l) >= len(d) else d

def najdaljsi_podpalindrom(niz):
    if len(niz) <= 1:
        return niz
    elif niz[0] == niz[-1]:
        return niz[0] +  najdaljsi_podpalindrom(niz[1:-1]) + niz[-1]
    else:
        levi = najdaljsi_podpalindrom(niz[:-1])
        desni = najdaljsi_podpalindrom(niz[1:])
        return levi if len(levi) >= len(desni) else desni

def stolpi(n):
    if n == 0:
        return 1
    return modri_stolpi(n) + rdeci_stolpi(n)

def modri_stolpi(n):
    if n == 0:
        return 1
    elif n < 0:
        return 0
    vsota = 0
    for k in [2, 3]:
        vsota += rdeci_stolpi(n - k)
    return vsota
    
def rdeci_stolpi(n):
    if n == 0:
        return 1
    elif n < 0:
        return 0
    vsota = 0
    for k in [1, 2]:
        vsota += modri_stolpi(n - k)
    return vsota

