def povej_kaj_se_dogaja(f):
    def gostobesedni_f(x):
        print(f"Računam {f.__name__}({x})")
        y = f(x)
        print(f"{f.__name__}({x}) = {y}")
        return y
    return gostobesedni_f

def memo(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            y = f(x)
            rezultati[x] = y
            print(f"Zapomnil sem si, da je {f.__name__}({x}) = {y}")
        return rezultati[x]
    return mem_f

def izracunaj_na_nic(f):
    return f(0)

@memo
@povej_kaj_se_dogaja
def kvadrat(x):
    return x ** 2

@memo
@povej_kaj_se_dogaja
def fakulteta(n):
    return 1 if n <= 1 else n * fakulteta(n - 1)

@memo
@povej_kaj_se_dogaja
def fib(n):
    return n if n <= 1 else fib(n - 1) + fib(n - 2)

@izracunaj_na_nic
def moja_funkcija(n):
    return 3 * n + 1

# mem_kvadrat = memo(kvadrat)
# glasni_kvadrat = povej_kaj_se_dogaja(kvadrat)
# fakulteta = povej_kaj_se_dogaja(fakulteta)


