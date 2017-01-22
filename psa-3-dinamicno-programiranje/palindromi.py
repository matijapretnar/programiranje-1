from functools import lru_cache

@lru_cache()
def najdaljsi_podpalindrom(niz):
    if len(niz) <= 1:
        return niz
    elif niz[0] == niz[-1]:
        return niz[0] + najdaljsi_podpalindrom(niz[1:-1]) + niz[-1]
    else:
        levi = najdaljsi_podpalindrom(niz[:-1])
        desni = najdaljsi_podpalindrom(niz[1:])
        if len(levi) < len(desni):
            return desni
        else:
            return levi

print(najdaljsi_podpalindrom('otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    'otorinolaring'
    ))