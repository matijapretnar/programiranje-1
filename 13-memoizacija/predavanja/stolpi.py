from functools import lru_cache

@lru_cache(maxsize=None)
def stevilo_stolpov(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return stevilo_stolpov(n - 1) + stevilo_stolpov(n - 2) + stevilo_stolpov(n - 3)

print(stevilo_stolpov(300))

def stevilo_stolpov_din(n):
    vmesne_visine = [1, 1, 2]
    for i in range(3, n + 1):
        vmesne_visine.append(
            vmesne_visine[i - 1] + vmesne_visine[i - 2] + vmesne_visine[i - 3]
        )
    return vmesne_visine[n]

print(stevilo_stolpov_din(300))

def stevilo_stolpov_mini(n):
    x, y, z = 1, 1, 2
    for _ in range(3, n + 1):
        x, y, z = y, z, x + y + z
    return z

print(stevilo_stolpov_mini(300))
