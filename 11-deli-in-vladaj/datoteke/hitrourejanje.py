def quicksort(sez):
    if len(sez) <= 1:
        return sez
    else:
        pivot = sez[0]
        manjsi = []
        vecji = []
        for x in sez[1:]:
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
