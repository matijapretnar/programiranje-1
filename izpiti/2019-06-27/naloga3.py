from functools import lru_cache

# containers, capacity = [1,4,16], 40
containers, capacity = [1, 3, 4, 7, 10], 300


def count_scenarios(containers, capacity):
    @lru_cache(maxsize=None)
    def count_scenarios_aux(k, cap):
        if cap == 0:
            return 1
        dim = len(containers)
        if k >= dim:
            return 0
        n = 0
        weight = containers[k]
        if weight <= cap:
            n += count_scenarios_aux(k, (cap - weight))
        n += count_scenarios_aux(k+1, cap)
        return n
    return (count_scenarios_aux(0, capacity))


# Rešitev za dodatne točke.
def incr_kth(l, k):
    l = [x for x in l]
    l[k] = l[k] + 1
    return l


def solutions(containers, capacity):
    @lru_cache(maxsize=None)
    def solutions_aux(k, capacity):
        if capacity == 0:
            return [[0 for i in range(len(containers))]]
        dim = len(containers)
        if k >= dim:
            return []
        weight = containers[k]
        sols_k = []
        if weight <= capacity:
            sols_k = solutions_aux(k, (capacity - weight))
            sols_k = [incr_kth(sol, k) for sol in sols_k]
        sols_no_k = solutions_aux(k+1, capacity)
        return sols_k + sols_no_k
    return solutions_aux(0, capacity)
