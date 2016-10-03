#!/usr/bin/python3

import heapq

# 3. naloga (Neurejen seznam)

def kti_najmanjsi(l, k, a=None, b=None):
    if a is None:
        a = 0
    if b is None:
        b = len(l) - 1
    pivot = l[a]
    za = a + 1
    ko = b
    i = a + 1
    while i <= b:
        if i > ko:
            break
        if l[i] <= pivot:
            l[za], l[i] = l[i], l[za]
            za += 1
            i += 1
        else:
            l[ko], l[i] = l[i], l[ko]
            ko -= 1
    l[a], l[za - 1] = l[za - 1], l[a]
    # Pivot je na mestu za - 1
    if za - a == k:
        return za - 1
    elif za - a < k:
        return kti_najmanjsi(l, k - (za - a), za, b)
    else:
        return kti_najmanjsi(l, k, a, za - 2)

def poisci(l, k, m):
    p = kti_najmanjsi(l, k + m - 1)
    h = []
    for i in range(p + 1):
        heapq.heappush(h, l[i])
        if len(h) > m:
            heapq.heappop(h)
    ret = []
    while len(h) > 0:
        ret.append(heapq.heappop(h))
    return ret

