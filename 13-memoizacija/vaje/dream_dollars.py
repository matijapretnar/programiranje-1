from functools import lru_cache

denominations = [1, 7, 10, 28]


def candidates(n):
    return [bill for bill in denominations if bill <= n]


def bills_greedy(n):
    if n == 0:
        return []
    cands = candidates(n)
    if cands == []:
        raise RuntimeError("no solution found")
    largest_candidate = max(cands)
    s = bills_greedy(n - largest_candidate)
    return s + [largest_candidate]


def bills_dyn_prog(n):
    if n == 0:
        return []
    cands = candidates(n)
    if cands == []:
        raise RuntimeError("no solution found")
    solutions = [bills_greedy(n - cand) + [cand] for cand in cands]
    return min(solutions, key=len)


def bills_iter(n):
    sols = [None] * (n+1)
    sols[0] = []
    for i in range(0, n+1):
        for bill in denominations:
            k = i - bill
            if k < 0:
                continue
            sk = sols[k]
            sb = [bill] + sk
            if sols[i] == None:
                sols[i] = sb
            elif len(sols[i]) <= len(sk):
                continue
            else:
                sols[i] = sb
    return sols[n]
