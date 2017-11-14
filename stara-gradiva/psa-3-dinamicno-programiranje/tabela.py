def stevilo_stolpov(n):
    modri = [1, 1, 1] + n * [0]
    rdeci = [1, 0, 1] + n * [0]
    for k in range(3, n + 1):
        modri[k] = rdeci[k - 1] + rdeci[k - 2]
        rdeci[k] = modri[k - 2] + modri[k - 3]
    return modri[n] + rdeci[n]

for i in range(1000):
    print(stevilo_stolpov(i))



LOKOMOTIVA
VLAKOVODJA