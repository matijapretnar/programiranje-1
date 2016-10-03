#!/usr/bin/python3

# 2. naloga (Drobencljava zaporedja)


# a)
# Časovna zahtevnost: O(n), kjer je n dolžina zaporedja.
def naloga2a(zap):
    st = dict()  # st[x] je število vseh zaporedij, ki se končajo na x.
    for x in zap:
        # Začetek novega zaporedja:
        st[x] = st.get(x, 0) + 1
        
        # Nadaljujemo lahko obstoječe zaporedje, ki se konča na x - 1 ali na x + 1:
        st[x] = st.get(x, 0) + st.get(x - 1, 0) + st.get(x + 1, 0)
        
    return sum(st.values())

print(naloga2a([1, 2, 3, 1]))

# b)
# Časovna zahtevnost: O(2^n * n^2)
# Načeloma je skoraj vsako podzaporedje danega zaporedja lahko drobencljavo, teh pa je O(2^n).

def naloga2b(zap):
    dro = dict()  # dro[x] je množica vseh drobnecljavih zaporedij, ki se končajo na x.
    
    for x in zap:  # O(n) * <notrajnost_zanke>
        if x not in dro:
            dro[x] = set()  # O(1)
        
        # Začetek novega zaporedja:
        dro[x].add((x, ))  # O(1)
    
        # Nadaljevanje zaporedja, ki se konča na x - 1:
        for z in dro.get(x - 1, set()):  # O(2^n) * <notranjost_zanke>
            dro[x].add(z + (x, ))  # O(n)
        
        # Nadaljevanje zaporedja, ki se konča na x + 1:
        for z in dro.get(x + 1, set()):
            dro[x].add(z + (x, ))
        
    return set.union(*dro.values())  # O(2^n * n)

print(naloga2b([1, 2, 3, 1]))

# c)
def naloga2c(zap):
    if not zap:
        return 0
    
    naj = 1  # Dolžina najdaljšega, ki smo ga odkrili do sedaj.
    zacetek = {zap[0]: 0}  # zacetek[x] je indeks prve pojavitve elementa x v trenutnem drobencljavem zaporedju.
    
    n = len(zap)
    for i in range(1, n):
        x = zap[i]
        if abs(x - zap[i - 1]) != 1:
            # Zaporedje je prekinjeno, začeti je treba znova.
            zacetek.clear()
            zacetek[x] = i
        else:
            # Saga se nadaljuje.
            if x not in zacetek:
                # Število x se pojavi prvič.
                zacetek[x] = i
            else:
                # Število se ne pojavi prvič.
                dolzina = i - zacetek[x] + 1
                naj = max(dolzina, naj)
    
    return naj

print(naloga2c([1, 2, 3, 2, 4, 3, 1]))
