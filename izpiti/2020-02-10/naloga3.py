from functools import lru_cache


def f(k, n):
    @lru_cache(maxsize=None)
    def count(a, k, n):
        if n == 1:
            return 1
        options = [count(x, k, n-1) for x in range(a-k, a+k+1) if x >= 0]
        return sum(options)
    return count(0, k, n)
