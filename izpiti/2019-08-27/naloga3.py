from functools import lru_cache

test = [
    [(1, 10), (3, -10)],    # 0
    [(2, 10), (5, -20)],    # 1
    [(3, -10)],   # 2
    [(4, 15)],  # 3
    [(5, 0)]]   # 4


def pobeg(pot):

    @lru_cache(maxsize=None)
    def pobeg(i, denar):
        if i >= len(pot) and denar >= 0:
            return [i]
        elif i >= len(pot):
            return None
        else:
            moznosti = []
            for (skok, stroski) in pot[i]:
                beg = pobeg(skok, denar + stroski)
                if beg is not None:
                    moznosti.append(beg)
            if len(moznosti) == 0:
                return None
            else:
                return [i] + sorted(moznosti, key=len)[0]

    return pobeg(0, 0)

print(pobeg(test))
