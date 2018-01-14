def povej_kaj_racunas(f):
    def glasni_f(x):
        print('Računam', x)
        return f(x)
    return glasni_f

@povej_kaj_racunas
def kvadriraj(x):
    return x ** 2

def memoiziraj(f):
    rezultati = {}
    def mem_f(*args):
        if args not in rezultati:
            rezultati[args] = f(*args)
        return rezultati[args]
    return mem_f

from functools import lru_cache
memoiziraj_najboljsi = lru_cache(max_size=None)

@memoiziraj
def fib(n):
    print(n)
    if n == 0 or n == 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)
