def naredi_glasno(f):
    def glasni_f(*args, **kwargs):
        print(f'Računam {f.__name__}({args}, {kwargs})')
        y = f(*args, **kwargs)
        print(f'Odgovor je {y}')
        return y
    return glasni_f

def cache(f):
    rezultati = {}
    def mem_f(x):
        print(f'Računam {f.__name__}({x})')
        if x in rezultati:
            print(f'Odgovor že poznam!')
            y = rezultati[x]
        else:          
            print(f'Odgovora še ne poznam!')
            y = f(x)
            rezultati[x] = y
        print(f'Odgovor je {y}')
        return y
    return mem_f


def kvadrat(x):
    return x ** 2

def vsota(x, y):
    return x + y

glasni_kvadrat = naredi_glasno(kvadrat)
glasni_kvadrat(5)

glasna_vsota = naredi_glasno(vsota)
glasna_vsota(5, 10)

print(80 * '-')
mem_kvadrat = cache(kvadrat)
print(80 * '-')
mem_kvadrat(5)
print(80 * '-')
mem_kvadrat(5)
print(80 * '-')
mem_kvadrat(5)

@cache
def kub(x):
    return x ** 3
print(80 * '-')
kub(5)
print(80 * '-')
kub(5)
print(80 * '-')
kub(5)
