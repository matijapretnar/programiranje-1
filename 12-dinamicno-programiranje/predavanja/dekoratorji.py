import time

def povej_kaj_racunas(f):
    def glasni_f(x):
        print('Računam', x)
        return f(x)
    return glasni_f

@povej_kaj_racunas
def kvadriraj(x):
    return x ** 2

kvadriraj(10)
kvadriraj(20)
kvadriraj(30)

def izpisi_se_porabljen_cas(f):
    def f_ki_izpise_se_porabljen_cas(x):
        zacetek = time.time()
        y = f(x)
        konec = time.time()
        print('Porabila sem {} ms'.format(1000 * (konec - zacetek)))
        return y
    return f_ki_izpise_se_porabljen_cas

@izpisi_se_porabljen_cas
def vsota(n):
    v = 0
    for i in range(n):
        v += i
    return v

print(vsota(10000))
print(vsota(10000))
print(vsota(100000))

