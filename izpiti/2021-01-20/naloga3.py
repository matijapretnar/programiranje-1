# =============================================================================
# Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
# gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
# Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
# fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
# prve fotografije in lokacijo druge fotografije kar se da velik vzpon.
#
# Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
# vrstnem redu, kot si sledijo po poti. Na primer:
#
#    [350, 230, 370, 920, 620, 80, 520, 780, 630]
#
# V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
# in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
# Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
# razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
# =============================================================================

# (a)
# -----------------------------------------------------------------------------
# Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
# višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
# utemeljite v komentarju.
# -----------------------------------------------------------------------------

def rozle_vrednost_deli_in_vladaj(xs):
    # O(n logn)

    def rozle(i, j):
        if i >= j-1:
            return 0

        half = (i+j) // 2
        both_left = rozle(i, half)
        both_right = rozle(half, j)
        different = max(xs[half:]) - min(xs[:half])

        return max(both_left, both_right, different)

    return rozle(0, len(xs))


def rozle_vrednost_pametno(xs):
    # O(n)

    cummin = []
    opt_min = xs[0]
    for x in xs:
        if x < opt_min:
            opt_min = x
        cummin.append(opt_min)

    print(cummin)

    return max([(x - opt_min) for (x, opt_min) in zip(xs, cummin)])

# (b)
# -----------------------------------------------------------------------------
# Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
# tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
# -----------------------------------------------------------------------------

def rozle_tocki_deli_in_vladaj(xs):
    # O(n logn)

    def index_of_min(i, j):
        best = xs[i]
        index = i
        for k in range(i+1, j):
            if xs[k] < best:
                best, index = xs[k], k
        return index

    def index_of_max(i, j):
        best = xs[i]
        index = i
        for k in range(i+1, j):
            if xs[k] > best:
                best, index = xs[k], k
        return index

    def rozle(i, j):
        if i >= j-1:
            return 0, i, i

        half = (i+j) // 2
        both_left = rozle(i, half)
        both_right = rozle(half, j)
        i_min = index_of_min(i, half)
        i_max = index_of_max(half, j)
        different = (xs[i_max] - xs[i_min], i_min, i_max)

        return max(both_left, both_right, different)

    val, i, j = rozle(0, len(xs))
    return i, j


def rozle_tocki_pametno(xs):
    # O(n)

    cummin = []
    opt_min = xs[0]
    opt_i = 0
    for k, x in enumerate(xs):
        if x < opt_min:
            opt_min = x
            opt_i = k
        cummin.append((opt_min, opt_i))

    options = [(x - opt_min, k, i) for ((i, x), (opt_min, k)) in
               zip(enumerate(xs), cummin)]

    val, i, j = max(options)
    return i, j
