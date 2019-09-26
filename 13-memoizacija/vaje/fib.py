from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n <= 1:
        return n
    else:
        x = fib(n-1)
        y = fib(n-2)
        return x + y

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.
@lru_cache()
def fib_cache(n):
    if n <= 1:
        return n
    else:
        x = fib_cache(n-1)
        y = fib_cache(n-2)
        return x + y

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.
def fib_memo_rec(n):
    fib_0_n = [None] * (max (2, n+1))
    def aux(n):
        if n < 2:
            return n
        else:
            if fib_0_n[n] == None:
                fib_0_n[n] = aux(n-1) + aux(n-2)
            return fib_0_n[n]
    return aux(n)

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
# Omejitev: Zmanjka spomina za > 10**5.
def fib_memo_iter(n):
    fib_0_n = [0] * (max (2, n+1))
    fib_0_n[1] = 1
    for i in range(2, n+1):
        fib_0_n[i] = fib_0_n[i-1] + fib_0_n[i-2]
    return
    return fib_0_n[n]

# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
# Omejitev: Deluje za > 10**6
def fib_iter(n):
    if n < 2:
        return n
    else:
        x_2 = 0
        x_1 = 1
        for i in range(2,n+1):
            x = x_1 + x_2
            x_2 = x_1
            x_1 = x
        return x
