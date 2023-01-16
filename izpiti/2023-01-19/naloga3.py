"""
Bacek Jon se nahaja pred gorskim grebenom, 
ki ga mora preplezati in se pri tem čim bolj 
najesti (trenutno še) živih zelišč, ki poraščajo 
gorovje.

Izgled grebena je predstavljen spodaj, kjer 
bacek začne povsem spodaj levo na točki z 
vrednostjo 1 in konča povsem desno na točki 
z vrednostjo 5, pri tem pa se lahko premika 
zgolj po nepraznih celicah (na posameznem 
kosu zemlje je vedno nenegativno število zelišč), 
kjer ga premik v desno/levo stane 10 enot energije, 
vertikalni 12, diagonalni pa 14 enot energije. 
V prvem delu se lahko bacek 
premika samo desno, ali po pripadajoči 
diagonali dessno navzgor, v drug polovici pa zgolj desno, 
ali pa desno navzdol.

    60  50  40  30  20  30  40  50  60  70
  40  50  60  73  80      40  60  30  20  40
10  20  30  40  50          10  20  90  40  50

V pythonu (ali OCamlu) to predstavimo s parom seznamov

[
 [60,50,40,30,20],
 [40,50,60,73,80],
 [10,20,30,40,50],
]
[
 [30,40,50,60,70],
 [40,60,30,20,40],
 [10,20,90,40,50],
]

Vaša naloga je, da pripravite program, ki bo za dani greben
izračunal največjo količino energije, ki jo ima lahko bacek 
ob koncu poti. Bacek ob premiku vsakič poje zelišča, ki so 
na lokaciji in s tem pridobi enako količino energije.
Predpostavite lahko, da bo neka pote čez greben vedno obstajala

"""

from functools import cache

matrix = list[list[int]]

def bacek_jon(up: matrix, down: matrix):
    horizontal_cost = 10
    vertical_cost = 12
    diagonal_cost = 14
    def solve(data: matrix, inverse: bool, initial_energy: int) -> int:
        h = len(data)
        w = len(data[0])
        @cache
        def solve(i : int, j: int, energy: int) -> tuple[list[str], int]:
            energy += data[i][j]
            # At the end
            if i == h-1 and j == w-1:
                return [], energy

            right = None, (None, -1)
            diagonal = None, (None, -1)
            up = None, (None, -1)

            # Can move right
            if j < w-1:
                right = "R", solve(i, j+1, energy - horizontal_cost)
            # Can move diagonal
            if i < h-1:
                diagonal = "X", solve(i+1, j, energy - diagonal_cost)
            if i < h - 2 and j > 0:
                up = ("U" if inverse else "D" ), solve(i+2, j-1, energy - vertical_cost)
            
            dir, (prev, energy) = max([right, diagonal, up], key=lambda x: x[1][1])

            return prev + [dir], energy
        
        return solve(0, 0, initial_energy)

    fst = solve(up[::-1], True, 0)
    snd = solve(down, False, fst[1] - horizontal_cost)

    return snd[1] + fst[1], fst[0][::-1] + ["R"] + snd[0][::-1]



print(
    bacek_jon([
 [60,50,40,30,20],
 [40,50,60,73,80],
 [10,20,30,40,50],
],
[
 [30,40,50,60,70],
 [40,60,30,20,40],
 [10,20,90,40,50],
]
    )
)

