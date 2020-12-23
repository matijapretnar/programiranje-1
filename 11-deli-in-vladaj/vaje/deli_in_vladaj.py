import random

###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end] (vključujoč oba robova).
#
# Primer: za [start = 1] in [end = 7] tabelo
#
#     [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 4 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
###############################################################################


def pivot(a, start, end):
    if start == end:
        return start

    pivot = a[start]
    first_bigger = start+1  # the index of the first element bigger than pivot

    for i in range(start+1, end+1):
        if a[i] < pivot:
            # switch smaller element with first bigger and update index
            a[first_bigger], a[i] = a[i], a[first_bigger]
            first_bigger += 1

    # switch pivot with last smaller (which is just before the first bigger)
    a[start], a[first_bigger-1] = a[first_bigger-1], a[start]
    return first_bigger-1


def pivot_alternative(a, start, end):
    p = a[start]
    left = start
    right = end
    
    while left < right:
        if a[left+1] <= p: # Move left inside
            left += 1
        elif p < a[right]: # Move right inside
            right -= 1
        else: # Swap
            a[left+1], a[right] = a[right], a[left+1]

    # Put pivot in the correct place
    a[start], a[left] = a[left], a[start]
    return left


def test_pivot(n, max_l, max_k):
    """ Performs a series of randomised tests on the pivot algorithm. """
    for _ in range(n):
        l = random.randint(0, max_l)
        a_orig = [random.randint(-max_k, max_k) for _ in range(l)]
        for j in range(l):
            a = a_orig[:]
            left = random.randint(0, j)
            right = random.randint(j, l-1)
            p = a[left]
            print(a, left, right)
            ind = pivot(a, left, right)
            if (a[ind] != p or any([j > p for j in a[left:ind]]) or any([j < p for j in a[ind:right+1]]) 
                or any(a[j] != a_orig[j] for j in range(0, left)) or any(a[j] != a_orig[j] for j in range(right+1, len(a)))) :
                return a_orig, a, left, right, ind

###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti element
# po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################


def kth_el_part(a, k, start, end):
    if start > end:
        return None
    else:
        pivot_i = pivot(a, start, end)
        if pivot_i == k:
            return a[pivot_i]
        elif pivot_i > k:
            return kth_el_part(a, k, start, pivot_i - 1)
        else:
            return kth_el_part(a, k, pivot_i + 1, end)


def kth_element(a, k):
    if k > len(a):
        return None
    else:
        return kth_el_part(a, k, 0, len(a)-1)


def test_quickselect(n, max_l, max_k):
    """ Performs a series of randomised tests on the pivot algorithm. """
    for _ in range(n):
        l = random.randint(0, max_l)
        a_orig = [random.randint(-max_k, max_k) for _ in range(l)]
        sor = sorted(a_orig)
        for j in range(l):
            a = a_orig[:]
            val = kth_element(a, j)
            if val != sor[j]:
                return a_orig, j, val, sor[j]


###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################


def quicksort_part(a, start, end):
    if start >= end:
        return
    else:
        pivot_i = pivot(a, start, end)
        quicksort_part(a, start, pivot_i - 1)
        quicksort_part(a, pivot_i + 1, end)
        return


def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return


def test_quicksort(n, max_l, max_k):
    """ Performs a series of randomised tests on the quicksort algorithm. """
    for _ in range(n):
        l = random.randint(0, max_l)
        a = [random.randint(-max_k, max_k) for _ in range(l)]
        a_quicksort = a[:]
        quicksort(a_quicksort)
        if a_quicksort != sorted(a):
            return a

###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
#
# Sestavite funkcijo [merge(target, list_1, list_2)], ki v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaka, naj bo prvi element iz prve tabele.
#
# Primer:
#
#     >>> list_1 = [1, 3, 5, 7, 10]
#     >>> list_2 = [1, 2, 3, 4, 5, 6, 7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> merge(target, list_1, list_2)
#     >>> target
#     [1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 10]
#
###############################################################################


def merge(target, list1, list2):
    # We assume lenghts of list1 and list2 exactly add up to that of target
    i1, i2 = 0, 0
    for j in range(len(target)):
        if (i2 >= len(list2)) or (i1 < len(list1) and list1[i1] <= list2[i2]):
            target[j] = list1[i1]
            i1 += 1
        else:
            target[j] = list2[i2]
            i2 += 1
    return

###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). Tabelo razdelimo na polovici,
# ju rekurzivno uredimo in nato zlijemo z uporabo funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja. Za
# razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je potrebno
# narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################


def mergesort(a):
    if len(a) <= 1:
        return
    else:
        half = len(a) // 2
        a1, a2 = a[:half], a[half:]
        mergesort(a1)
        mergesort(a2)
        merge(a, a1, a2)
        return


def test_mergesort(n, max_l, max_k):
    """ Performs a series of randomised tests on the mergesort algorithm. """
    for _ in range(n):
        l = random.randint(0, max_l)
        a = [random.randint(-max_k, max_k) for _ in range(l)]
        a_mergesort = a[:]
        mergesort(a_mergesort)
        if a_mergesort != sorted(a):
            return a
