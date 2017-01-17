##########################################################################
# Izbiranje k-tega elementa 
#
# V pomoč pri reševanju nalog sta na voljo videa:
# 
# * [Premetavanje elementov preko pivota](http://vimeo.com/31025494)
# * [Iskanje k-tega elementa](http://vimeo.com/31027120)
#
# Pravimo, da je element a[k] v tabeli a *pivot*, če za vse i < k
# velja a[i] <= a[k] in za vse i > k velja a[i] > a[k]. Se pravi,
# da so vsi elementi pred k-tim manjši od ali enaki a[k] in vsi
# elementi za k-tim večji od a[k].
# 
# Dana je neprazna tabela a, v kateri bi radi preuredili elemente tako,
# da bo a[0] postal pivot. To pomeni, da moramo dati vse elemente, ki
# so manjši od ali enaki a[0] na začetek tabele, nato sledi a[0] in
# nato še elementi, ki so večji od a[0]. Na primer, tabelo
# 
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
# 
# preuredimo v
# 
# [4, 5, 2, 0, 10, 15, 15, 17, 18]
# 
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
# 
# Sestavi funkcijo premeci(a), ki preuredi tabelo a tako, da bo a[0]
# postal pivot. Funkcija naj vrne indeks, na katerem je po preurejanju
# pristal pivot. Funkcija naj deluje v času O(n), kjer je n dolžina
# tabele a. Primer:
# 
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> premeci(a)
#     4
#     >>> a
#     [4, 5, 2, 0, 10, 15, 15, 17, 18]
##########################################################################



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
# Sestavite funkcijo po_velikosti(a, k), ki v tabeli a poišče
# k-ti element po velikosti. Funkcija sme spremeniti tabelo a.
##########################################################################

