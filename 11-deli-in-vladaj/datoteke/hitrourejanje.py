depth = 0

def debug(f):
    def debug_f(*args):
        global depth
        print("{}{}({})".format(depth * " ", f.__name__, ", ".join(repr(arg) for arg in args)))
        depth += 2
        res = f(*args)
        depth -= 2
        return res
    return debug_f

@debug
def fakulteta(n):
    if n == 0:
        return 1
    else:
        return n * fakulteta(n - 1)

# print(fakulteta(10))

@debug
def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

# fib(10)


# @debug
def quicksort(sez):
    if len(sez) <= 1:
        return sez
    else:
        n = len(sez)
        pivot = sez[n // 2]
        manjsi = []
        vecji = []
        for x in sez[:n // 2] + sez[n // 2 + 1:]:
            if x <= pivot:
                manjsi.append(x)
            else:
                vecji.append(x)
        return quicksort(manjsi) + [pivot] + quicksort(vecji)


def premeci(sez, i, j):
    assert j - i > 1
    pivot = i
    i += 1
    j -= 1
    while i < j:
        while i < j and sez[i] <= sez[pivot]:
            i += 1
        while i < j and sez[j] > sez[pivot]:
            j -= 1
        sez[i], sez[j] = sez[j], sez[i]
    if sez[i] <= sez[pivot]:
        novi_pivot = i
    else:
        novi_pivot = i - 1
    sez[pivot], sez[novi_pivot] = sez[novi_pivot], sez[pivot]
    return novi_pivot


def quicksort_na_mestu(sez, i=0, j=None):
    if j is None:
        j = len(sez)
    if j - i <= 1:
        return
    else:
        pivot = premeci(sez, i, j)
        quicksort_na_mestu(sez, i, pivot)
        quicksort_na_mestu(sez, pivot + 1, j)
