##########################################################################
# Dana je neprazna tabela a, v kateri bi radi preuredili elemente tako,
# da bo a[0] postal pivot. To pomeni, da moramo dati vse elemente, ki
# so manjši od ali enaki a[0] na začetek tabele, nato sledi a[0] in
# nato še elementi, ki so večji od a[0]. Na primer, tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo pivot_list(a), ki preuredi tabelo a tako, da bo a[0]
# postal pivot. Funkcija naj vrne indeks, na katerem je po preurejanju
# pristal pivot. Funkcija naj deluje v času O(n), kjer je n dolžina
# tabele a. Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot_list(a)
#     4
#     >>> a
#     [0, 2, 5, 4, 10, 11, 17, 15, 18]
##########################################################################

def pivot_list(a) :
    # Shrani pivot
    pivot = a[0]
    # Shrani kazalca
    front_i = 0
    back_i = len(a) - 1
    # Premikaj kazalca in zamenjaj elemente če potrebno
    while front_i != back_i :
        if a[front_i + 1] <= pivot :
            front_i += 1
        elif a[back_i] > pivot :
            back_i -= 1
        else:
            temp = a[front_i + 1]
            a[front_i + 1] = a[back_i]
            a[back_i] = temp
    # Premakni pivot na pravo mesto
    a[0] = a[front_i]
    a[front_i] = pivot
    # Vrni indeks pivota
    return front_i

##########################################################################
# Tabelo a želimo urediti z algoritmom 'quicksort', ki smo ga spoznali
# na predavanjih.
#
# Napišite funkcijo quicksort(a), ki uredi tabelo a s pomočjo pivotiranja.
# Pri tem za poenostavitev ni potrebno, da uporabljate zgolj eno tabelo.
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##########################################################################

def quicksort(a):
    # Ni več potrebno urejati.
    if len(a) <= 1:
        return a
    # Uredi podtabele in združi v rezultat.
    else:
        p_i = pivot_list(a)
        return quicksort(a[:p_i]) + [a[p_i]] + quicksort(a[p_i + 1:])

##########################################################################
# V tabeli a želimo poiskati vrednost k-tega elementa po velikosti.
# Na primer, če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši
# elementi 2, 3 in 4. Pri tem štejemo indekse od 0 naprej, se pravi
# "ničti" element je 2.
#
# Sestavite funkcijo kth_element(a, k), ki v tabeli a poišče k-ti element
# po velikosti. Funkcija sme spremeniti tabelo a.
##########################################################################

def kth_element(a, k):
    # Preveri če je naloga nemogoča.
    if k > len(a):
        return None

    #Pivotiraj
    pivot_index = pivot_list(a)
    #Poglej v katerem od podseznamov nadaljujemo iskanje, če je potrebno.
    if pivot_index == k:
        return a[k]
    elif pivot_index > k:
        return kth_element(a[:pivot_index], k)
    else:
        return kth_element(a[pivot_index+1:], k - pivot_index - 1)
