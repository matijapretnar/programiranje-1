
def cache(f):
    rezultati = {}
    def mem_f(*args):
        if args not in rezultati:
            rezultati[args] = f(*args)
        return rezultati[args]
    return mem_f

@cache
def najdaljsa_pot(mat, i=0, j=0):
    ...

def najdaljsa_pot(mat):
    @cache
    def pomozna(i, j):
        ...
    return pomozna(0, 0)