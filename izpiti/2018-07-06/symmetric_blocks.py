from functools import lru_cache

# Reverse a string / list
def rev (w) :
    return w[::-1]

def is_palindrome (w) :
    return w == rev(w)

def is_increasing (w) :
    if w == [] :
        return True
    t = True
    z = w[0]
    for x in w[1:] :
        t = x >= z
        z = x
        if not t :
            return False
    return t

def sum_symmetric (w) :
    if len(w) <= 1 :
        return True
    l = [int(c) for c in w]
    n = int (len(l) / 2)
    return sum(l[:n]) == sum(l[n:])


@lru_cache(maxsize=None)
def number_of_blocks (w, is_symmetric) :

    if w == [] :
        return 0

    if is_symmetric(w) :
        return 1

    options = [number_of_blocks(w[:i], is_symmetric) +
               number_of_blocks(w[i:], is_symmetric) for i in range(1, len(w))]

    n = min(options)

    return n

@lru_cache(maxsize=None)
def blocks (w, f) :
    # print(w)

    if len(w) == 0 :
        return (0, [w])

    if f(w) :
        return (1, [w])

    options = None
    for i in range(1, len(w)):
        nl, wl = blocks(w[:i], f)
        nr, wr = blocks(w[i:], f)
        k, ws = nl + nr, wl + wr

        if options == None:
            options = (k, ws)

        else:
            m, l = options
            if k < m :
                options = (k, ws)

    return options
