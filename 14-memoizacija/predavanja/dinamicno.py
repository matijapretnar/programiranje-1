from functools import lru_cache
memoiziraj = lru_cache(maxsize=None)


@memoiziraj
def najdaljsi_podpalindrom(niz):
    if len(niz) <= 1:
        return niz
    elif niz[0] == niz[-1]:
        return niz[0] +  najdaljsi_podpalindrom(niz[1:-1]) + niz[-1]
    else:
        levi = najdaljsi_podpalindrom(niz[:-1])
        desni = najdaljsi_podpalindrom(niz[1:])
        return levi if len(levi) >= len(desni) else desni


def pot_z_najmanjso_vsoto(matrika):
    @memoiziraj
    def pot(vrs, sto):
        '''Išče vrednost najcenejše poti od (0, 0) do (vrs, sto)'''
        if vrs == sto == 0:
            return matrika[0][0]
        elif vrs == 0:
            return matrika[vrs][sto] + pot(vrs, sto - 1)
        elif sto == 0:
            return matrika[vrs][sto] + pot(vrs - 1, sto)
        else:
            return matrika[vrs][sto] + min(
                pot(vrs - 1, sto),
                pot(vrs, sto - 1)
            )
    return pot(len(matrika) - 1, len(matrika[0]) - 1)
