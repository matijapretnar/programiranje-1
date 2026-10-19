# =============================================================================
# Ker ne zna ničesar koristnega, se je Miha odločil, da bo postal vplivnež. 
# Priskrbel si je zemljevid plaže, ki za vsako mesto na plaži
# pove, koliko sledilcev dobi (ali izgubi), če objavi fotografijo s tega mesta. 
# Plažo predstavimo s pravokotno mrežo dimenzije `M * N`, kjer za vsako celico 
# povemo, koliko sledilcev bo Miha dobil, če se na poti prek te celice slika.
# Miha svojo pot začne v točki `(0, 0)`, konča v točki `(M-1, N-1)`, na svoji 
# poti do cilja pa bi rad nabral čim več sledilcev, pri čemer med potjo nikoli 
# ne sme zaiti izven plaže.
# Miha se lahko običajno premika na tri načine: korak desno, korak navzdol, 
# korak desno-navzdol in pri tem objavi slike iz vseh lokacij
# na svoji poti (tudi če so njihove vrednosti negativne). Poleg osnovnih korakov 
# lahko "največ enkrat" na svoji poti naredi tudi 
# korak nazaj (torej se vrne na polje, kjer je bil trenutek prej). 
# Ker sledilci nimajo dobrega spomina, se lahko Miha večkrat slika na isti lokaciji 
# in vsakič dobi (ali izgubi) podano število sledilcev.
# 
# Definirajte funkcijo, ki sprejme zemljevid plaže in vrne maksimalno število sledilcev, 
# ki jih Miha lahko nabere na podani plaži.
# Miho zanima zgolj končna sprememba sledilcev, zato je ta lahko skupno tudi negativna.
# 
# Na spodnji mreži je najvplivnejši sprehod (1, 2, 5, 30, 5, 30, -1, 5) vreden 77 sledilcev. 
# =============================================================================

piran = [
    [1, 2,  -3, -10, 9],
    [0, 0,   5,   5, 2],
    [1, 2,  30,  -1, 0],
    [4, 3, -20,  -1, 5],
]