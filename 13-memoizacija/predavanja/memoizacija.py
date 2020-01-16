def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f

@memoiziraj
def kvadrat(n):
    print(f'{n}^2 = {n ** 2}')
    return n ** 2

from functools import lru_cache

@lru_cache(maxsize=None)
def fib(n):
    print(n, end='-')
    if n == 0 or n == 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

def najcenejsa_pot(matrika):
    '''Cena najcenejše ↓/→ poti od zgoraj levo do spodaj desno'''
    @lru_cache(maxsize=None)
    def pomozna(vrs, sto):
        '''Cena najcenejše ↓/→ poti od matrika[0][0] do matrika[vrs][sto]'''
        print(vrs, sto)
        if vrs == 0 and sto == 0:
            return matrika[vrs][sto]
        else:
            moznosti = []
            if vrs != 0:
                # do matrika[vrs][sto] sem prišel iz matrika[vrs - 1][sto]
                moznosti.append(pomozna(vrs - 1, sto))
            if sto != 0:
                # do matrika[vrs][sto] sem prišel iz matrika[vrs][sto - 1]
                moznosti.append(pomozna(vrs, sto - 1))
            return matrika[vrs][sto] + min(moznosti)
    return pomozna(len(matrika) - 1, len(matrika[0]) - 1)

matrika = [
    [131, 673, 234, 103, 18],
    [201, 96, 342, 965, 150],
    [630, 803, 746, 422, 111],
    [537, 699, 497, 121, 956],
    [805, 732, 524, 37, 331]
]
