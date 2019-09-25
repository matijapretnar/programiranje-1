from functools import lru_cache

def frog_escape(swamp):
    @lru_cache(maxsize=None)
    def escape(k, e):
        if k >= len(swamp):
            return 0
        else:
            e += swamp[k]
            return 1 + min([escape(k + d, e - d) for d in range(1, e + 1)])
    return escape(0, 0)

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]  # Should be 3
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]  # Should be 2
