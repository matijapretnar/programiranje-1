def podvoji(x):
    print("Računam", x)
    return 2 * x

print(podvoji(10))
print(podvoji(10))

rezultati = {}
def mem_podvoji(x):
    if x in rezultati:
        return rezultati[x]
    else:
        y = podvoji(x)
        rezultati[x] = y
        return y

def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x in rezultati:
            return rezultati[x]
        else:
            y = f(x)
            rezultati[x] = y
            return y
    return mem_f

@memoiziraj
def stevilo_stolpov(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return stevilo_stolpov(n - 1) + stevilo_stolpov(n - 2) + stevilo_stolpov(n - 3)

stevilo_stolpov = memoiziraj(stevilo_stolpov)

def izracunaj_na_nic(f):
    return f(0)

@izracunaj_na_nic
def g(x):
    return 2 * x + 3
