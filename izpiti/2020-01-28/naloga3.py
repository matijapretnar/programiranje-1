from functools import lru_cache

# a
@lru_cache(maxsize=None)
def blocky(n, m, l):
    if m <= 0:
        return 1
    elif n < l:
        return 0
    else:
        return blocky(n-1, m, l) + blocky(n-l-1, m-1, l)


# b
def blocko(n, blocks):
    @lru_cache(maxsize=None)
    def blocker(n, i):
        if i >= len(blocks) and n >= -1:
            return 1
        elif n <= 0:
            return 0
        else:
            return blocker(n-1, i) + blocker(n-blocks[i]-1, i+1)
    return blocker(n, 0)
