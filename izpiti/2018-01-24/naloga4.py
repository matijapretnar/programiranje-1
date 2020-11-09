#  ==========================================================================
#  NALOGA 4.1
#
#  Lisjaček krade jabolka v sadovnjaku, kjer so jablane razporejene v vrstah.
#  Vsaka jablana ima različno število jabolk, ki jih lisjaček lahko doseže.
#  Okrade lahko zgolj N jablan preden ga prežene lastnik sadovnjaka.
#
#  Ker je lisjaček še mlad in nevešč kraje jabolk, v vsaki vrsti obira
#  jablane po vrsti, ko pa se odloči da se bo lotil naslednje vrste se ne
#  vrača več nazaj. V primeru, da se znajde na koncu zadnje vrste svojo
#  avanturo zaključi, ne glede na to ali ima na voljo še kakšno obiranje.
#
#  Napišite algoritem, ki izračuna največje število jabolk, ki jih lisjaček
#  lahko ukrade. Sadovnjak predstavimo z matriko, kjer vrste predstavljajo
#  vrste sadovnjaka in vrednosti v matriki število jabolk na jablani.
#  V smislu matrike se lahko lisjaček premika zgolj v desno ali pa na začetek
#  nove vrstice, kjer za vsak premik porabi en korak.
#
#  Primer: | 1 2 0 5 |      N = 6       ------>  17
#          | 0 4 1 1 |
#          | 8 0 4 2 |
#
#  Za vse točke mora biti funkcija učinkovita (ne eksponentna časovna
#  zahtevnost).
#
#  ==========================================================================


from functools import lru_cache

test_matrix = [[1, 2, 0, 5], [0, 4, 1, 1], [8, 0, 4, 2]]


def max_points(matrix, max_steps):

    @lru_cache(maxsize=None)
    def jumper(r, c, k):
        val = matrix[r][c]
        # No more steps
        if (k == 0):
            return 0
        # Hit boundaries
        elif (r == len(matrix) - 1):
            # Can't go down
            if (c == len(matrix[r]) - 1):
                # Can't go right
                return val
            else:
                # Can go right
                return val + jumper(r, c+1, k-1)
        else:
            # Can go down
            if (c == len(matrix[r]) - 1):
                # Can't go right
                return val + jumper(r+1, 0, k-1)
            else:
                # Can go right
                return val + max(jumper(r, c+1, k-1), jumper(r+1, 0, k-1))

    # Call function
    return jumper(0, 0, max_steps)
