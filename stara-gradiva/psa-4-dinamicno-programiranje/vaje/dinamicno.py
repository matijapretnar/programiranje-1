# Miška
# =====
#
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
# desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne mase. Miška bi se rada kar se da nažrla, zato jo zanima, katero pot
# naj ubere. Napišite funkcijo max_sircka(matrika_sirckov), ko dobi matriko z
# masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.

def miska(board):
    height, width = len(board), len(board[0])
    def empty():
        return [[ None for x in range(width) ] for y in range(height) ]
    val, mov = empty(), empty()
    def mem(x,y):
        def pick(d,r):
            if r > d :
                return (r, '>')
            else :
                return (d, 'v')
        if (x == width or y == height) :
            return 0
        if val[y][x] == None :
            right = mem(x+1, y)
            down  = mem(x, y+1)
            (v, m) = pick(down, right)
            val[y][x] = board[y][x] + v
            mov[y][x] = m
        return val[y][x]
    mem(0,0)
    return val, mov

b4 = [[0, 1, 0, 1],
      [0, 1, 3, 2],
      [0, 2, 0, 3]]

b1 = [[0]]

def string_of_board(b) :
    return '\n'.join([ "|" + ' '.join([ str(v) for v in r ]) + "|" for r in b ])

# Nahrbtnik
# =========
#
# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega k kilogramov. (Podelili so
# tri nagrade in sicer s parametrom k = 1, k = 2 in k = 5). Napišite funkcijo
# nahrbtnik(seznam_artiklov, k), ki poišče največjo ceno, ki jo lahko odnesemo
# iz trgovine. Naredite dve verziji: pri prvi lahko vzamemo samo en artikel iste
# vrste, pri drugi pa poljubno število artiklov iste vrste.

izdelki = [
	('jogurt', 0.39, 0.18),
	('mleko', 0.89, 1.03),
    ('kava', 2.19, 0.2),
    ('maslo', 1.49, 0.25),
    ('kvas', 0.22, 0.042),
    ('jajca', 2.39, 0.69),
    ('klobasa', 3.76, 0.50),
    ('čebula', 1.29, 2.0),
    ('kruh', 2.99, 1.0),
    ('Nutella', 4.99, 0.75),
    ('sok', 1.15, 2.0)
]

items = [['', 60, 10],
         ['', 100, 20],
         ['', 120, 30]]


def nahrbtnik(items, capacity) :
    scale_weight, scale_value = 1000,100
    capacity = capacity * scale_weight
    N = len(items)
    def weight(i):
        return int(items[i][2] * scale_weight)
    def value(i):
        return int(items[i][1] * scale_value)
    res = [ [ 0 for j in range(capacity+1) ] for i in range(N) ]

    for i in range(N) :
        for j in range(capacity+1) :
            w = weight(i)
            if w > j :
                res[i][j] = res[i-1][j] # can't add, too heavy
            else :
                v = value(i)
                res[i][j] = max(res[i-1][j],
                                res[i-1][j - w] + v)

    j = capacity
    picks = []
    for i in range(N-1, 0, -1) :
        if res[i][j] != res[i-1][j] :
            picks.append(items[i])
            j -= weight(i)
    picks.reverse()
    return res[N-1][capacity] / scale_value, picks

# Jajca
# =====
#
# Živimo v visoki stolpnici, ki ima n nadstropij. Imamo škatlo k jajc, ki so menda zelo trpežna,
# saj naj bi prenesla padce z višjih nadstropij stoplnice. Radi bi ugotovili, katero je najvišje
# nadstopje, pri katerem jajca še preživijo padec. Ker nimamo veliko časa, bi radi poiskali
# strategijo, pri kateri bomo minimizirali število metov.
#
# Razmislite:
#  * Kako moramo ravnati v primeru, ko imamo samo eno jajce?
#  * Kako lahko ravnamo v primeru, ko imamo na voljo zelo veliko jajc (več kot je število
#    nadstropij)?
#
# Napišite funkcij, ki bo izračunala maksimalno število metov (v najslabšem primeru), da ugotovimo
# številko kritičnega nadstropja, če imamo na voljo točko k jajc.
